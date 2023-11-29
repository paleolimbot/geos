
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "geos-common.h"
#include "libgeos.h"
#include "wk-v1.h"

#define handle globalHandle

#define HANDLE_OR_RETURN(expr) \
  result = expr;               \
  if (result != WK_CONTINUE) return result

#define HANDLE_CONTINUE_OR_BREAK(expr) \
  result = expr;                       \
  if (result == WK_ABORT_FEATURE)      \
    continue;                          \
  else if (result == WK_ABORT)         \
  break

int geos_wk_read_point(const GEOSGeometry* g, uint32_t part_id, wk_handler_t* handler) {
  int result;
  wk_meta_t meta;
  WK_META_RESET(meta, WK_POINT);

  if (GEOSHasZ_r(handle, g)) {
    meta.flags |= WK_FLAG_HAS_Z;
  }

  int srid = GEOSGetSRID_r(handle, g);
  if (srid != 0) {
    meta.srid = srid;
  }

  meta.precision = GEOSGeom_getPrecision_r(handle, g);

  if (GEOSisEmpty_r(handle, g)) {
    meta.size = 0;
  } else {
    meta.size = 1;
  }

  HANDLE_OR_RETURN(handler->geometry_start(&meta, part_id, handler->handler_data));
  if (meta.size) {
    double coord[4];
    GEOSGeomGetX_r(handle, g, coord + 0);
    GEOSGeomGetY_r(handle, g, coord + 1);
    if (meta.flags & WK_FLAG_HAS_Z) {
      GEOSGeomGetZ_r(handle, g, coord + 2);
    }
    HANDLE_OR_RETURN(handler->coord(&meta, coord, 0, handler->handler_data));
  }
  return handler->geometry_end(&meta, part_id, handler->handler_data);
}

int geos_wk_read_linestring(const GEOSGeometry* g, uint32_t part_id,
                            wk_handler_t* handler) {
  int result;
  wk_meta_t meta;
  WK_META_RESET(meta, WK_LINESTRING);

  if (GEOSHasZ_r(handle, g)) {
    meta.flags |= WK_FLAG_HAS_Z;
  }

  int srid = GEOSGetSRID_r(handle, g);
  if (srid != 0) {
    meta.srid = srid;
  }

  meta.precision = GEOSGeom_getPrecision_r(handle, g);

  if (GEOSisEmpty_r(handle, g)) {
    meta.size = 0;
  } else {
    meta.size = GEOSGetNumCoordinates_r(handle, g);
  }

  HANDLE_OR_RETURN(handler->geometry_start(&meta, part_id, handler->handler_data));
  if (meta.size && (meta.flags & WK_FLAG_HAS_Z)) {
    double coord[4];
    const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(handle, g);
    for (uint32_t i = 0; i < meta.size; i++) {
      GEOSCoordSeq_getX_r(handle, seq, i, coord + 0);
      GEOSCoordSeq_getY_r(handle, seq, i, coord + 1);
      GEOSCoordSeq_getZ_r(handle, seq, i, coord + 2);
      HANDLE_OR_RETURN(handler->coord(&meta, coord, i, handler->handler_data));
    }
  } else if (meta.size) {
    double coord[4];
    const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(handle, g);
    for (uint32_t i = 0; i < meta.size; i++) {
      GEOSCoordSeq_getX_r(handle, seq, i, coord + 0);
      GEOSCoordSeq_getY_r(handle, seq, i, coord + 1);
      HANDLE_OR_RETURN(handler->coord(&meta, coord, i, handler->handler_data));
    }
  }
  return handler->geometry_end(&meta, part_id, handler->handler_data);
}

int geos_wk_read_ring(const GEOSGeometry* r, uint32_t ring_id, const wk_meta_t* meta,
                      wk_handler_t* handler) {
  int result;
  double coord[4];
  uint32_t size;

  const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(handle, r);
  GEOSCoordSeq_getSize_r(handle, seq, &size);

  HANDLE_OR_RETURN(handler->ring_start(meta, size, ring_id, handler->handler_data));
  for (uint32_t i = 0; i < size; i++) {
    GEOSCoordSeq_getX_r(handle, seq, i, coord + 0);
    GEOSCoordSeq_getY_r(handle, seq, i, coord + 1);
    HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
  }
  return handler->ring_end(meta, size, ring_id, handler->handler_data);
}

int geos_wk_read_ring_z(const GEOSGeometry* r, uint32_t ring_id, const wk_meta_t* meta,
                        wk_handler_t* handler) {
  int result;
  double coord[4];
  uint32_t size;

  const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(handle, r);
  GEOSCoordSeq_getSize_r(handle, seq, &size);

  HANDLE_OR_RETURN(handler->ring_start(meta, size, ring_id, handler->handler_data));
  for (uint32_t i = 0; i < size; i++) {
    GEOSCoordSeq_getX_r(handle, seq, i, coord + 0);
    GEOSCoordSeq_getY_r(handle, seq, i, coord + 1);
    GEOSCoordSeq_getZ_r(handle, seq, i, coord + 2);
    HANDLE_OR_RETURN(handler->coord(meta, coord, i, handler->handler_data));
  }
  return handler->ring_end(meta, size, ring_id, handler->handler_data);
}

int geos_wk_read_polygon(const GEOSGeometry* g, uint32_t part_id, wk_handler_t* handler) {
  int result;
  wk_meta_t meta;
  WK_META_RESET(meta, WK_POLYGON);

  if (GEOSHasZ_r(handle, g)) {
    meta.flags |= WK_FLAG_HAS_Z;
  }

  int srid = GEOSGetSRID_r(handle, g);
  if (srid != 0) {
    meta.srid = srid;
  }

  meta.precision = GEOSGeom_getPrecision_r(handle, g);

  int n_interior_rings = GEOSGetNumInteriorRings_r(handle, g);
  if (GEOSisEmpty_r(handle, g)) {
    meta.size = 0;
  } else {
    meta.size = n_interior_rings + 1;
  }

  HANDLE_OR_RETURN(handler->geometry_start(&meta, part_id, handler->handler_data));
  if (meta.size && (meta.flags & WK_FLAG_HAS_Z)) {
    geos_wk_read_ring_z(GEOSGetExteriorRing_r(handle, g), 0, &meta, handler);
    for (int i = 0; i < n_interior_rings; i++) {
      geos_wk_read_ring_z(GEOSGetInteriorRingN_r(handle, g, i), i + 1, &meta, handler);
    }
  } else if (meta.size) {
    geos_wk_read_ring(GEOSGetExteriorRing_r(handle, g), 0, &meta, handler);
    for (int i = 0; i < n_interior_rings; i++) {
      geos_wk_read_ring(GEOSGetInteriorRingN_r(handle, g, i), i + 1, &meta, handler);
    }
  }
  return handler->geometry_end(&meta, part_id, handler->handler_data);
}

// definition needed by read_collection()
int geos_wk_read_geometry(const GEOSGeometry* g, uint32_t part_id, wk_handler_t* handler);

int geos_wk_read_collection(const GEOSGeometry* g, int geos_type, uint32_t part_id,
                            wk_handler_t* handler) {
  int result;

  // type integers are identical for GEOS and WK for collection types
  wk_meta_t meta;
  WK_META_RESET(meta, geos_type);
  meta.size = GEOSGetNumGeometries_r(handle, g);

  if (GEOSHasZ_r(handle, g)) {
    meta.flags |= WK_FLAG_HAS_Z;
  }

  int srid = GEOSGetSRID_r(handle, g);
  if (srid != 0) {
    meta.srid = srid;
  }

  meta.precision = GEOSGeom_getPrecision_r(handle, g);

  HANDLE_OR_RETURN(handler->geometry_start(&meta, part_id, handler->handler_data));
  for (int i = 0; i < meta.size; i++) {
    HANDLE_OR_RETURN(geos_wk_read_geometry(GEOSGetGeometryN_r(handle, g, i), i, handler));
  }
  return handler->geometry_end(&meta, part_id, handler->handler_data);
}

int geos_wk_read_geometry(const GEOSGeometry* g, uint32_t part_id,
                          wk_handler_t* handler) {
  int geos_type = GEOSGeomTypeId_r(handle, g);
  switch (geos_type) {
    case GEOS_POINT:
      return geos_wk_read_point(g, part_id, handler);
    case GEOS_LINESTRING:
    case GEOS_LINEARRING:
      return geos_wk_read_linestring(g, part_id, handler);
    case GEOS_POLYGON:
      return geos_wk_read_polygon(g, part_id, handler);
    case GEOS_MULTIPOINT:
    case GEOS_MULTILINESTRING:
    case GEOS_MULTIPOLYGON:
    case GEOS_GEOMETRYCOLLECTION:
      return geos_wk_read_collection(g, geos_type, part_id, handler);
    default:
      return handler->error("Unrecognized geometry type",
                            handler->handler_data);  // # nocov
  }
}

SEXP geos_wk_read_geos_geometry(SEXP geom, wk_handler_t* handler) {
  R_xlen_t size = Rf_xlength(geom);

  wk_vector_meta_t vector_meta;
  WK_VECTOR_META_RESET(vector_meta, WK_GEOMETRY);
  vector_meta.size = size;
  vector_meta.flags |= WK_FLAG_DIMS_UNKNOWN;

  if (handler->vector_start(&vector_meta, handler->handler_data) == WK_CONTINUE) {
    SEXP item;
    GEOSGeometry* itemGeometry;
    int result;
    for (R_xlen_t i = 0; i < size; i++) {
      if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();

      HANDLE_CONTINUE_OR_BREAK(
          handler->feature_start(&vector_meta, i, handler->handler_data));
      item = VECTOR_ELT(geom, i);
      if (item == R_NilValue) {
        HANDLE_CONTINUE_OR_BREAK(handler->null_feature(handler->handler_data));
      } else {
        itemGeometry = (GEOSGeometry*)R_ExternalPtrAddr(item);
        if ((itemGeometry == NULL) &&
            (handler->error("GEOSGeometry* is NULL", handler->handler_data) !=
             WK_ABORT)) {
          continue;
        }

        HANDLE_CONTINUE_OR_BREAK(
            geos_wk_read_geometry(itemGeometry, WK_PART_ID_NONE, handler));
      }

      if (handler->feature_end(&vector_meta, i, handler->handler_data) != WK_CONTINUE) {
        break;
      }
    }
  }

  return handler->vector_end(&vector_meta, handler->handler_data);
}

SEXP geos_c_wk_read_geos_geometry(SEXP geom, SEXP handler_xptr) {
  return wk_handler_run_xptr(&geos_wk_read_geos_geometry, geom, handler_xptr);
}
