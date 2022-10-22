
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <memory>
#include <vector>
#include <sstream>

#include "geos-common.h"
#include "geoarrow-imports.h"
#include "libgeos.h"
#include "wk-v1.h"

#define handle globalHandle

#define CPP_START                         \
    char cpp_exception_error[8096];       \
    memset(cpp_exception_error, 0, 8096); \
    try {

#define CPP_END                                           \
    } catch (std::exception& e) {                         \
        strncpy(cpp_exception_error, e.what(), 8096 - 1); \
    }                                                     \
    Rf_error("%s", cpp_exception_error);                  \
    return R_NilValue;


// The other versions of CPP_START and CPP_END stack-allocate the
// error message buffer, which takes a non-trivial amount of time
// when done at this scale (at worst 4 times per coordinate). By
// keeping the buffer in the handler_data struct, we can call C++
// from every handler method without measurable overhead.
#define WK_METHOD_CPP_START                     \
    try {

#define WK_METHOD_CPP_END                                 \
    } catch (std::exception& e) {                         \
        strncpy(data->cpp_exception_error, e.what(), 8096 - 1); \
    }                                                     \
    Rf_error("%s", data->cpp_exception_error);            \
    return R_NilValue;

#define WK_METHOD_CPP_END_INT                                 \
    } catch (std::exception& e) {                         \
        strncpy(data->cpp_exception_error, e.what(), 8096 - 1); \
    }                                                     \
    Rf_error("%s", data->cpp_exception_error);            \
    return WK_ABORT;


class GEOSGeometryWrapper {
public:
    GEOSGeometryWrapper(): ptr(nullptr) {}
    ~GEOSGeometryWrapper() {
        if (ptr != nullptr) {
            GEOSGeom_destroy_r(handle, ptr);
        }
    }

    GEOSGeometry* release() {
        GEOSGeometry* result = ptr;
        ptr = nullptr;
        return result;
    }

    GEOSGeometry* ptr;
};

class GEOSCoordSeqWrapper {
public:
    GEOSCoordSeqWrapper(): ptr(nullptr) {}
    ~GEOSCoordSeqWrapper() { reset(); }

    void reset() {
      if (ptr != nullptr) {
        GEOSCoordSeq_destroy_r(handle, ptr);
      }
      ptr = nullptr;
    }

    GEOSCoordSequence* release() {
        GEOSCoordSequence* result = ptr;
        ptr = nullptr;
        return result;
    }

    GEOSCoordSequence* ptr;
};

class Constructor : public util::Handler {
 public:
  Constructor(): coord_size_(2), coords_(nullptr), coords_size_(0), coords_capacity_(0),
   last_feature_(nullptr), srid_(WK_SRID_NONE) {}

  ~Constructor() {
    if (coords_ != nullptr) {
      free(coords_);
    }
  }

  void set_srid(uint32_t srid) {
    srid_ = srid;
  }

  void new_dimensions(util::Dimensions dims) {
    has_z_ = false;
    has_m_ = false;

    switch (dims) {
    case util::Dimensions::XYZ:
        has_z_ = true;
        break;
    case util::Dimensions::XYM:
        has_m_ = true;
        break;
    case util::Dimensions::XYZM:
        has_z_ = true;
        has_m_ = true;
        break;
    default:
      break;
    }

    coord_size_ = 2 + has_z_ + has_m_;
  }

  Result feat_start() {
    geometry_type_.clear();
    parts_.clear();
    last_feature_.reset();
    return Result::CONTINUE;
  }

  Result null_feat() {
    return Result::ABORT_FEATURE;
  }

  Result geom_start(util::GeometryType geometry_type, int64_t size) {
    geometry_type_.push_back(geometry_type);
    coords_size_ = 0;

    switch (geometry_type) {
    case util::GeometryType::POINT:
    case util::GeometryType::LINESTRING:
      if (size > 0) {
        coords_reserve(coord_size_ * size);
      }
      break;
    case util::GeometryType::POLYGON:
    case util::GeometryType::MULTIPOINT:
    case util::GeometryType::MULTILINESTRING:
    case util::GeometryType::MULTIPOLYGON:
    case util::GeometryType::GEOMETRYCOLLECTION:
      parts_.emplace_back();
      if (size > 0) {
        parts_.back().reserve(size);
      }
      break;
    default:
      throw std::runtime_error("Unsupported geometry type");
    }

    return Result::CONTINUE;
  }

  Result ring_start(int64_t size) {
    coords_size_ = 0;
    if (size > 0) {
      coords_reserve(size * coord_size_);
    }

    return Result::CONTINUE;
  }

  Result coords(const double* coord, int64_t n, int32_t coord_size) {
    size_t size_after = coords_size_ + (n * coord_size);
    coords_reserve(size_after);
    memcpy(coords_ + coords_size_, coord, n * coord_size * sizeof(double));
    coords_size_ += n * coord_size;
    return Result::CONTINUE;
  }

  Result ring_end() {
    parts_.back().push_back(finish_geom(GEOS_LINEARRING));
    return Result::CONTINUE;
  }

  Result geom_end() {
    util::GeometryType geometry_type = geometry_type_.back();
    geometry_type_.pop_back();

    auto geom = finish_geom(geos_geometry_type(geometry_type));
    if (parts_.empty()) {
      last_feature_.swap(geom);
    } else {
      parts_.back().push_back(std::move(geom));
    }

    return Result::CONTINUE;
  }

  std::unique_ptr<GEOSGeometryWrapper> finish() {
    return std::move(last_feature_);
  }

  void coords_reserve(size_t capacity) {
    if (capacity > coords_capacity_) {
      coords_ = reinterpret_cast<double*>(realloc(coords_, capacity * sizeof(double)));
      if (coords_ == nullptr) {
        throw std::runtime_error("Failed to reallocate coordinates");
      }
      coords_capacity_ = capacity;
    }
  }

 private:
  std::vector<util::GeometryType> geometry_type_;
  int coord_size_;
  bool has_z_;
  bool has_m_;
  double* coords_;
  size_t coords_size_;
  size_t coords_capacity_;
  GEOSCoordSeqWrapper seq_;
  std::vector<std::vector<std::unique_ptr<GEOSGeometryWrapper>>> parts_;
  std::vector<GEOSGeometry*> parts_back_cache_;
  std::unique_ptr<GEOSGeometryWrapper> last_feature_;
  uint32_t srid_;

  std::unique_ptr<GEOSGeometryWrapper> finish_geom(int geos_geom_type) {
    std::unique_ptr<GEOSGeometryWrapper> geom(new GEOSGeometryWrapper());
    std::pair<GEOSGeometry**, size_t> part_pointers;

    switch (geos_geom_type) {
    case GEOS_POINT:
      finish_points();
      geom->ptr = GEOSGeom_createPoint_r(handle, seq_.release());
      break;
    case GEOS_LINESTRING:
      finish_points();
      geom->ptr = GEOSGeom_createLineString_r(handle, seq_.release());
      break;
    case GEOS_LINEARRING:
      finish_points();
      geom->ptr = GEOSGeom_createLinearRing_r(handle, seq_.release());
      break;
    case GEOS_POLYGON:
      if (parts_.back().size() == 0) {
        parts_.pop_back();
        geom->ptr = GEOSGeom_createEmptyPolygon_r(handle);
      } else {
        part_pointers = pop_and_release_parts_back();
        geom->ptr = GEOSGeom_createPolygon_r(
          handle, part_pointers.first[0], part_pointers.first + 1, part_pointers.second - 1);
      }
      break;
    case GEOS_MULTIPOINT:
    case GEOS_MULTILINESTRING:
    case GEOS_MULTIPOLYGON:
    case GEOS_GEOMETRYCOLLECTION:
      part_pointers = pop_and_release_parts_back();
      geom->ptr = GEOSGeom_createCollection_r(
        handle, geos_geom_type, part_pointers.first, part_pointers.second);
      break;
    default:
      throw std::runtime_error("Unsupported geometry type");
    }

    if (geom->ptr == nullptr) {
      std::stringstream ss;
      ss << "Error creating geometry: " << globalErrorMessage;
      throw std::runtime_error(ss.str().c_str());
    }

    if (srid_ != WK_SRID_NONE) {
      GEOSSetSRID_r(handle, geom->ptr, srid_);
    }

    return geom;
  }

  void finish_points() {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 10, 0)
    if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 10, 0)) {
      finish_points_compat();
      return;
    }

    seq_.reset();
    seq_.ptr = GEOSCoordSeq_copyFromBuffer_r(
        handle, coords_, coords_size_ / coord_size_, has_z_, has_m_);
    if (seq_.ptr == nullptr) {
      throw std::runtime_error(globalErrorMessage);
    }

    coords_size_ = 0;
#else
    finish_points_compat();
#endif
  }

  void finish_points_compat() {
    seq_.reset();
    size_t n_coords = coords_size_ / coord_size_;

    seq_.ptr = GEOSCoordSeq_create_r(handle, n_coords, 2 + has_z_);
    if (seq_.ptr == nullptr) {
      throw std::runtime_error(globalErrorMessage);
    }

    double* coord = coords_;
    if (has_z_) {
      for (size_t i = 0; i < n_coords; i++) {
        GEOSCoordSeq_setXYZ_r(handle, seq_.ptr, i, coord[0], coord[1], coord[2]);
        coord += coord_size_;
      }
    } else {
      for (size_t i = 0; i < n_coords; i++) {
        GEOSCoordSeq_setXY_r(handle, seq_.ptr, i, coord[0], coord[1]);
        coord += coord_size_;
      }
    }

    coords_size_ = 0;
  }

  std::pair<GEOSGeometry**, size_t> pop_and_release_parts_back() {
    std::pair<GEOSGeometry**, size_t> result;

    std::vector<std::unique_ptr<GEOSGeometryWrapper>> parts_back = std::move(parts_.back());
    parts_.pop_back();

    parts_back_cache_.resize(parts_back.size());
    for (size_t i = 0; i < parts_back.size(); i++) {
        parts_back_cache_[i] = parts_back[i]->release();
    }

    result.first = parts_back_cache_.data();
    result.second = parts_back.size();
    return result;
  }

  static int geos_geometry_type(util::GeometryType geometry_type) {
    switch (geometry_type) {
    case util::GeometryType::POINT: return GEOS_POINT;
    case util::GeometryType::LINESTRING: return GEOS_LINESTRING;
    case util::GeometryType::POLYGON: return GEOS_POLYGON;
    case util::GeometryType::MULTIPOINT: return GEOS_MULTIPOINT;
    case util::GeometryType::MULTILINESTRING: return GEOS_MULTILINESTRING;
    case util::GeometryType::MULTIPOLYGON: return GEOS_MULTIPOLYGON;
    case util::GeometryType::GEOMETRYCOLLECTION: return GEOS_GEOMETRYCOLLECTION;
    default:
      throw std::runtime_error("Unsupported geometry type");
    }
  }
};

typedef struct {
    Constructor* builder;
    SEXP result;
    R_xlen_t feat_id;
    int coord_size;
    util::Dimensions dims;
    char cpp_exception_error[8096];
} builder_handler_t;


// TODO: Both of these allocate in a way that could longjmp and possibly leak memory
static inline void builder_result_append(builder_handler_t* data, SEXP value) {
    R_xlen_t current_size = Rf_xlength(data->result);
    if (data->feat_id >= current_size) {
        SEXP new_result = PROTECT(Rf_allocVector(VECSXP, current_size * 2 + 1));
        for (R_xlen_t i = 0; i < current_size; i++) {
            SET_VECTOR_ELT(new_result, i, VECTOR_ELT(data->result, i));
        }
        R_ReleaseObject(data->result);
        data->result = new_result;
        R_PreserveObject(data->result);
        UNPROTECT(1);
    }

    SET_VECTOR_ELT(data->result, data->feat_id, value);
    data->feat_id++;
}

static inline void builder_result_finalize(builder_handler_t* data) {
    R_xlen_t current_size = Rf_xlength(data->result);
    if (data->feat_id != current_size) {
        SEXP new_result = PROTECT(Rf_allocVector(VECSXP, data->feat_id));
        for (R_xlen_t i = 0; i < data->feat_id; i++) {
            SET_VECTOR_ELT(new_result, i, VECTOR_ELT(data->result, i));
        }
        R_ReleaseObject(data->result);
        data->result = new_result;
        R_PreserveObject(data->result);
        UNPROTECT(1);
    }
}

int builder_vector_start(const wk_vector_meta_t* meta, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;

  if (data->result != R_NilValue) {
      Rf_error("Destination vector was already allocated"); // # nocov
  }

  if (meta->size == WK_VECTOR_SIZE_UNKNOWN) {
      data->result = PROTECT(Rf_allocVector(VECSXP, 1024));
  } else {
      data->result = PROTECT(Rf_allocVector(VECSXP, meta->size));
  }
  R_PreserveObject(data->result);
  UNPROTECT(1);

  data->dims = util::Dimensions::DIMENSIONS_UNKNOWN;
  data->feat_id = 0;

  return WK_CONTINUE;
}

SEXP builder_vector_end(const wk_vector_meta_t* meta, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  builder_result_finalize(data);
  Rf_setAttrib(data->result, R_ClassSymbol, Rf_mkString("geos_geometry"));
  return data->result;
}

int builder_feature_start(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  WK_METHOD_CPP_START
  data->builder->feat_start();
  return WK_CONTINUE;
  WK_METHOD_CPP_END_INT
}

int builder_feature_null(void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  builder_result_append(data, R_NilValue);
  return WK_ABORT_FEATURE;
}

int builder_feature_end(const wk_vector_meta_t* meta, R_xlen_t feat_id, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  WK_METHOD_CPP_START
  auto feat = data->builder->finish();
  SEXP feature_xptr = PROTECT(geos_common_geometry_xptr(feat->release()));
  builder_result_append(data, feature_xptr);
  UNPROTECT(1);
  return WK_CONTINUE;
  WK_METHOD_CPP_END_INT
}

int builder_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  WK_METHOD_CPP_START

  auto geometry_type = static_cast<util::GeometryType>(meta->geometry_type);

  int32_t size;
  if (meta->size == WK_SIZE_UNKNOWN) {
      size = -1;
  } else {
      size = meta->size;
  }

  util::Dimensions dims;
  if (meta->flags & WK_FLAG_HAS_Z && meta->flags & WK_FLAG_HAS_M) {
    data->coord_size = 4;
    dims = util::Dimensions::XYZM;
  } else if (meta->flags & WK_FLAG_HAS_Z) {
    data->coord_size = 3;
    dims = util::Dimensions::XYZ;
  } else if (meta->flags & WK_FLAG_HAS_M) {
    data->coord_size = 3;
    dims = util::Dimensions::XYM;
  } else {
    data->coord_size = 2;
    dims = util::Dimensions::XY;
  }

  if (dims != data->dims) {
    data->builder->new_dimensions(dims);
    data->dims = dims;
  }

  data->builder->set_srid(meta->srid);

  data->builder->geom_start(geometry_type, size);
  return WK_CONTINUE;
  WK_METHOD_CPP_END_INT
}

int builder_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  WK_METHOD_CPP_START
  data->builder->geom_end();
  return WK_CONTINUE;
  WK_METHOD_CPP_END_INT
}

int builder_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  WK_METHOD_CPP_START

  if (size == WK_SIZE_UNKNOWN) {
      data->builder->ring_start(-1);
  } else {
      data->builder->ring_start(size);
  }

  return WK_CONTINUE;
  WK_METHOD_CPP_END_INT
}

int builder_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  WK_METHOD_CPP_START
  data->builder->ring_end();
  return WK_CONTINUE;
  WK_METHOD_CPP_END_INT
}

int builder_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id, void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  WK_METHOD_CPP_START
  data->builder->coords(coord, 1, data->coord_size);
  return WK_CONTINUE;
  WK_METHOD_CPP_END_INT
}

int builder_error(const char* message, void* handler_data) {
  Rf_error("%s", message);
  return WK_ABORT;
}

void builder_deinitialize(void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  if (data->result != R_NilValue) {
    R_ReleaseObject(data->result);
    data->result = R_NilValue;
  }
}

void builder_finalize(void* handler_data) {
  builder_handler_t* data = (builder_handler_t*) handler_data;
  if (data != nullptr) {
    free(data);
  }
}

void delete_vector_constructor(SEXP xptr) {
    auto ptr = reinterpret_cast<Constructor*>(R_ExternalPtrAddr(xptr));
    if (ptr != nullptr) {
        delete ptr;
    }
}

extern "C" SEXP geos_c_geos_writer_new(void) {
  CPP_START

  auto builder = new Constructor();
  SEXP builder_xptr = PROTECT(R_MakeExternalPtr(builder, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(builder_xptr, &delete_vector_constructor);

  wk_handler_t* handler = wk_handler_create();

  handler->vector_start = &builder_vector_start;
  handler->vector_end = &builder_vector_end;

  handler->feature_start = &builder_feature_start;
  handler->null_feature = &builder_feature_null;
  handler->feature_end = &builder_feature_end;

  handler->geometry_start = &builder_geometry_start;
  handler->geometry_end = &builder_geometry_end;

  handler->ring_start = &builder_ring_start;
  handler->ring_end = &builder_ring_end;

  handler->coord = &builder_coord;

  handler->error = &builder_error;

  handler->deinitialize = &builder_deinitialize;
  handler->finalizer = &builder_finalize;

  builder_handler_t* data = (builder_handler_t*) malloc(sizeof(builder_handler_t));
  if (data == NULL) {
    wk_handler_destroy(handler); // # nocov
    Rf_error("Failed to alloc handler data"); // # nocov
  }

  data->coord_size = 2;
  data->builder = builder;
  data->result = R_NilValue;
  memset(data->cpp_exception_error, 0, 8096);

  handler->handler_data = data;

  // include the builder pointer as a tag for this external pointer
  // which guarnatees that it will not be garbage collected until
  // this object is garbage collected
  SEXP handler_xptr = wk_handler_create_xptr(handler, builder_xptr, R_NilValue);
  UNPROTECT(1);
  return handler_xptr;

  CPP_END
}
