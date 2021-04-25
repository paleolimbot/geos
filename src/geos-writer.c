#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include <memory.h>

#include "geos-common.h"
#include "libgeos.h"
#include "wk-v1.h"

#define handle geos_gc_handle

#define GEOS_MAX_RECURSION_DEPTH 32
#define GEOS_WRITER_GEOM_LENGTH GEOS_MAX_RECURSION_DEPTH + 2
#define GEOS_INITIAL_SIZE_IF_UNKNOWN 32

typedef struct {
    // output vector list()
    SEXP geos;
    R_xlen_t feat_id;

    // container geometries and position within the container
    // each item is an array of GEOSGeometry* that must be malloced
    // and freed
    uint32_t recursion_level;
    GEOSGeometry** geom[GEOS_WRITER_GEOM_LENGTH];    
    int part_id[GEOS_WRITER_GEOM_LENGTH];
    int geom_size[GEOS_WRITER_GEOM_LENGTH];
    
    // the current coordinate sequence and position within the sequence
    // this method is simpler than managing a GEOSCoordSequence*
    // and will be compatible with the future GEOSCoordSeq_copyFromBuffer()
    double* coord_seq;
    uint32_t coord_seq_size;

    int coord_size;
    uint32_t coord_id;
} geos_writer_t;

geos_writer_t* geos_writer_new() {
    geos_writer_t* writer = (geos_writer_t*) malloc(sizeof(geos_writer_t));
    if (writer == NULL) {
        return NULL;
    }

    writer->geos = R_NilValue;
    writer->recursion_level = 0;
    writer->coord_seq = (double*) malloc(1024 * sizeof(double));
    if (writer->coord_seq == NULL) {
        free(writer); // # nocov
        return NULL; // # nocov
    }

    writer->coord_seq_size = 1024;
    writer->coord_size = 2;
    writer->coord_id = 0;

    for (int i = 0; i < GEOS_WRITER_GEOM_LENGTH; i++) {
        writer->geom[i] = NULL;
        writer->part_id[i] = 0;
        writer->geom_size[i] = 0;
    }

    return writer;
}

static inline void geos_writer_geos_append(geos_writer_t* writer, SEXP value) {
    R_xlen_t current_size = Rf_xlength(writer->geos);
    if (writer->feat_id >= current_size) {
        SEXP new_result = PROTECT(Rf_allocVector(VECSXP, current_size * 2 + 1));
        for (R_xlen_t i = 0; i < current_size; i++) {
            SET_VECTOR_ELT(new_result, i, VECTOR_ELT(writer->geos, i));
        }
        R_ReleaseObject(writer->geos);
        writer->geos = new_result;
        R_PreserveObject(writer->geos);
        UNPROTECT(1);
    }

    SET_VECTOR_ELT(writer->geos, writer->feat_id, value);
    writer->feat_id++;
}

static inline void geos_writer_geos_finalize(geos_writer_t* writer) {
    R_xlen_t current_size = Rf_xlength(writer->geos);
    if (writer->feat_id != current_size) {
        SEXP new_result = PROTECT(Rf_allocVector(VECSXP, writer->feat_id));
        for (R_xlen_t i = 0; i < writer->feat_id; i++) {
            SET_VECTOR_ELT(new_result, i, VECTOR_ELT(writer->geos, i));
        }
        R_ReleaseObject(writer->geos);
        writer->geos = new_result;
        R_PreserveObject(writer->geos);
        UNPROTECT(1);
    }
}

static inline void geos_writer_coord_seq_append(geos_writer_t* writer, const double* coord) {
    if ((writer->coord_id * writer->coord_size) >= writer->coord_seq_size) {
        uint32_t new_size = writer->coord_seq_size * 2 + 1;
        double* new_coord_seq = (double*) realloc(writer->coord_seq, new_size);
        if (new_coord_seq == NULL) {
            Rf_error("Failed to realloc coordinate sequence"); // # nocov
        }
        free(writer->coord_seq);
        writer->coord_seq = new_coord_seq;
        writer->coord_seq_size = new_size;
    }

    memcpy(
        writer->coord_seq + (writer->coord_id * writer->coord_size),
        coord,
        writer->coord_size * sizeof(double)
    );

    writer->coord_id++;
}

static inline GEOSCoordSequence* geos_writer_coord_seq_finalize(geos_writer_t* writer) {
    int geos_coord_size = writer->coord_size;
    if (geos_coord_size > 3) {
        geos_coord_size = 3;
    }

    GEOSCoordSequence* seq = GEOSCoordSeq_create_r(handle, writer->coord_id, geos_coord_size);
    if (seq == NULL) {
        Rf_error(globalErrorMessage); // # nocov
    }

    double* coord;
    if (geos_coord_size >= 3) {
        for (uint32_t i = 0; i < writer->coord_id; i++) {
            coord = writer->coord_seq + (writer->coord_size * i);
            GEOSCoordSeq_setXYZ_r(handle, seq, i, coord[0], coord[1], coord[2]);
        }
    } else {
        for (uint32_t i = 0; i < writer->coord_id; i++) {
            coord = writer->coord_seq + (writer->coord_size * i);
            GEOSCoordSeq_setXY_r(handle, seq, i, coord[0], coord[1]);
        }
    }

    return seq;
}

static inline void geos_writer_geom_append(geos_writer_t* writer, GEOSGeometry* g) {
    int level = writer->recursion_level;
    if ((level >= GEOS_MAX_RECURSION_DEPTH) || (level < 0)) {
        Rf_error("Invalid recursion depth");
    }
    
    if (writer->geom[level] == NULL) {
        writer->geom[level] = 
          (GEOSGeometry**) malloc(GEOS_INITIAL_SIZE_IF_UNKNOWN * sizeof(GEOSGeometry**));
        memset(writer->geom[level], 0, GEOS_INITIAL_SIZE_IF_UNKNOWN * sizeof(GEOSGeometry**));
        writer->geom_size[level] = GEOS_INITIAL_SIZE_IF_UNKNOWN;
    }

    if (writer->part_id[level] >= writer->geom_size[level]) {
        int current_size = writer->geom_size[level];
        GEOSGeometry** new_seq = malloc((current_size * 2 + 1) * sizeof(GEOSGeometry**));
        memset(new_seq, 0, (current_size * 2 + 1) * sizeof(GEOSGeometry**));
        for (int i = 0; i < writer->part_id[level]; i++) {
            new_seq[i] = writer->geom[level][i];
        }
        free(writer->geom[level]);
        writer->geom[level] = new_seq;
    }

    writer->geom[level][writer->part_id[level]] = g;
    writer->part_id[level]++;
}

static inline void geos_writer_geom_release(geos_writer_t* writer) {
    for (int i = 0; i < writer->part_id[writer->recursion_level]; i++) {
        writer->geom[writer->recursion_level][i] = NULL;
    }
}

int geos_writer_vector_start(const wk_vector_meta_t* vector_meta, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;

    if (writer->geos != R_NilValue) {
        Rf_error("Destination vector was already allocated"); // # nocov
    }

    if (vector_meta->size == WK_VECTOR_SIZE_UNKNOWN) {
        writer->geos = PROTECT(Rf_allocVector(VECSXP, 1024));
    } else {
        writer->geos = PROTECT(Rf_allocVector(VECSXP, vector_meta->size));
    }

    R_PreserveObject(writer->geos);
    UNPROTECT(1);

    writer->feat_id = 0;

    return WK_CONTINUE;
}

int geos_writer_feature_start(const wk_vector_meta_t* vector_meta, R_xlen_t feat_id, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    writer->recursion_level = 0;
    return WK_CONTINUE;
}

int geos_writer_null_feature(void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    geos_writer_geos_append(writer, R_NilValue);
    return WK_ABORT_FEATURE;
}

int geos_writer_geometry_start(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    writer->recursion_level++;

    writer->coord_id = 0;
    if (meta->flags & WK_FLAG_HAS_Z) {
        writer->coord_size = 3;
    } else {
        writer->coord_size = 2;
    }

    writer->part_id[writer->recursion_level] = 0;

    return WK_CONTINUE;
}

int geos_writer_ring_start(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    writer->recursion_level++;
    writer->coord_id = 0;
    return WK_CONTINUE;
}

int geos_writer_coord(const wk_meta_t* meta, const double* coord, uint32_t coord_id, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    geos_writer_coord_seq_append(writer, coord);
    return WK_CONTINUE;
}

int geos_writer_ring_end(const wk_meta_t* meta, uint32_t size, uint32_t ring_id, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    GEOSCoordSequence* seq = geos_writer_coord_seq_finalize(writer);
    GEOSGeometry* geom = GEOSGeom_createLinearRing_r(handle, seq);

    if (geom == NULL) {
        Rf_error(globalErrorMessage);
    }

    writer->recursion_level--;

    if (writer->recursion_level == 0) {
        Rf_error("Can't add ring as a top-level geometry");
    }

    geos_writer_geom_append(writer, geom);
    return WK_CONTINUE;
}

int geos_writer_geometry_end(const wk_meta_t* meta, uint32_t part_id, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;

    GEOSGeometry* geom = NULL;
    GEOSCoordSequence* seq = NULL;
    switch (meta->geometry_type) {
    case WK_POINT:
        seq = geos_writer_coord_seq_finalize(writer);
        geom = GEOSGeom_createPoint_r(handle, seq);
        break;
    case WK_LINESTRING:
        seq = geos_writer_coord_seq_finalize(writer);
        geom = GEOSGeom_createLineString_r(handle, seq);
        break;
    case WK_POLYGON:
        if (writer->part_id[writer->recursion_level] == 0) {
            geom = GEOSGeom_createEmptyPolygon_r(handle);
        } else if (writer->part_id[writer->recursion_level] == 1) {
            geom = GEOSGeom_createPolygon_r(
                handle,
                writer->geom[writer->recursion_level][0],
                NULL,
                0
            );
        } else {
            geom = GEOSGeom_createPolygon_r(
                handle,
                writer->geom[writer->recursion_level][0],
                writer->geom[writer->recursion_level] + 1,
                writer->part_id[writer->recursion_level] - 1
            );
        }

        geos_writer_geom_release(writer);
        break;
    case WK_MULTIPOINT:
    case WK_MULTILINESTRING:
    case WK_MULTIPOLYGON:
    case WK_GEOMETRYCOLLECTION:
        geom = GEOSGeom_createCollection_r(
            handle,
            meta->geometry_type,
            writer->geom[writer->recursion_level],
            writer->part_id[writer->recursion_level]
        );
        geos_writer_geom_release(writer);
        break;
    default:
        Rf_error("Unknown geometry type: %d", meta->geometry_type);
    }

    if (geom == NULL) {
        Rf_error(globalErrorMessage);
    }

    writer->recursion_level--;

    if (writer->recursion_level == 0) {
        geos_writer_geos_append(writer, geos_common_geometry_xptr(geom));
    } else {
        geos_writer_geom_append(writer, geom);
    }

    return WK_CONTINUE;
}

SEXP geos_writer_vector_end(const wk_vector_meta_t* vector_meta, void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    geos_writer_geos_finalize(writer);
    Rf_setAttrib(writer->geos, R_ClassSymbol, Rf_mkString("geos_geometry"));
    return writer->geos;
}

void geos_writer_deinitialize(void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;

    if (writer->geos != R_NilValue) {
        R_ReleaseObject(writer->geos);
        writer->geos = R_NilValue;
    }

    GEOSGeometry* g;
    for (int i = 0; i < (GEOS_WRITER_GEOM_LENGTH); i++) {
        if (writer->geom[i] != NULL) {
            for (int j = 0; j < writer->geom_size[i]; j++) {
                g = writer->geom[i][j];
                if (g != NULL) {
                    GEOSGeom_destroy_r(handle, g);
                }
            }
            free(writer->geom[i]);
            writer->geom[i] = NULL;
        }
    }
}

void geos_writer_finalize(void* handler_data) {
    geos_writer_t* writer = (geos_writer_t*) handler_data;
    if (writer != NULL) {
        if (writer->coord_seq != NULL) {
            free(writer->coord_seq);
            writer->coord_seq = NULL;
        }
        free(writer);
    }
}

SEXP geos_c_geos_writer_new() {
    wk_handler_t* handler = wk_handler_create();

    handler->finalizer = &geos_writer_finalize;
    handler->vector_start = &geos_writer_vector_start;
    handler->feature_start = &geos_writer_feature_start;
    handler->null_feature = &geos_writer_null_feature;
    handler->geometry_start = &geos_writer_geometry_start;
    handler->ring_start = &geos_writer_ring_start;
    handler->coord = &geos_writer_coord;
    handler->ring_end = &geos_writer_ring_end;
    handler->geometry_end = &geos_writer_geometry_end;
    handler->vector_end = &geos_writer_vector_end;
    handler->deinitialize = &geos_writer_deinitialize;

    handler->handler_data = geos_writer_new();
    if (handler->handler_data == NULL) {
        wk_handler_destroy(handler); // # nocov
        Rf_error("Failed to alloc handler data"); // # nocov
    }

    return wk_handler_create_xptr(handler, R_NilValue, R_NilValue);
}
