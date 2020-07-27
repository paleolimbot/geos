
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

#define GEOS_UNARY_GEOMETRY(_func)                             \
  R_xlen_t size = Rf_xlength(geom);                            \
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));         \
                                                               \
  GEOS_INIT();                                                 \
                                                               \
  SEXP item;                                                   \
  GEOSGeometry* geometry;                                      \
  GEOSGeometry* geometryResult;                                \
  for (R_xlen_t i = 0; i < size; i++) {                        \
    item = VECTOR_ELT(geom, i);                                \
                                                               \
    if (item == R_NilValue) {                                  \
      SET_VECTOR_ELT(result, i, R_NilValue);                   \
      continue;                                                \
    }                                                          \
                                                               \
    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);        \
    GEOS_CHECK_GEOMETRY(geometry, i);                          \
                                                               \
    geometryResult = _func(handle, geometry);                  \
                                                               \
    if (geometryResult == NULL) {                              \
      UNPROTECT(1);                                            \
      GEOS_ERROR("[i=%d] ", i + 1);                            \
    } else {                                                   \
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));\
    }                                                          \
  }                                                            \
                                                               \
  GEOS_FINISH();                                               \
  UNPROTECT(1);                                                \
  return result;


SEXP geos_c_centroid(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSGetCentroid_r);
}

SEXP geos_c_boundary(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSBoundary_r);
}

SEXP geos_c_minimum_width(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSMinimumWidth_r);
}

SEXP geos_c_minimum_clearance_line(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSMinimumClearanceLine_r);
}

SEXP geos_c_minimum_rotated_rectagle(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSMinimumRotatedRectangle_r);
}

SEXP geos_c_unary_union(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSUnaryUnion_r);
}

SEXP geos_c_coverage_union(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSCoverageUnion_r);
}

SEXP geos_c_point_on_surface(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSPointOnSurface_r);
}

SEXP geos_c_node(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSNode_r);
}

SEXP geos_c_make_valid(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSMakeValid_r);
}

SEXP geos_c_unique_points(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSGeom_extractUniquePoints_r);
}

SEXP geos_c_reverse(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSReverse_r);
}

SEXP geos_c_merge_lines(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSLineMerge_r);
}

SEXP geos_c_build_area(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSBuildArea_r);
}

SEXP geos_c_envelope(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSEnvelope_r);
}

SEXP geos_c_convex_hull(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSConvexHull_r);
}

SEXP geos_c_point_start(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSGeomGetStartPoint_r);
}

SEXP geos_c_point_end(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSGeomGetEndPoint_r);
}

SEXP geos_c_clone(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSGeom_clone_r);
}


#define GEOS_UNARY_GEOMETRY_PARAM(_func, _param_scalar, _param_ptr, _na_check)        \
  R_xlen_t size = Rf_xlength(geom);                                        \
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));                     \
  _param_scalar* paramPtr = _param_ptr(param);                             \
  GEOS_INIT();                                                             \
                                                                           \
  SEXP item;                                                               \
  GEOSGeometry* geometry;                                                  \
  GEOSGeometry* geometryResult;                                            \
  for (R_xlen_t i = 0; i < size; i++) {                                    \
    item = VECTOR_ELT(geom, i);                                            \
                                                                           \
    if (item == R_NilValue || _na_check) {                                 \
      SET_VECTOR_ELT(result, i, R_NilValue);                               \
      continue;                                                            \
    }                                                                      \
                                                                           \
    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);                    \
    GEOS_CHECK_GEOMETRY(geometry, i);                                      \
                                                                           \
    geometryResult = _func(handle, geometry, paramPtr[i]);                 \
                                                                           \
    if (geometryResult == NULL) {                                          \
      UNPROTECT(1);                                                        \
      GEOS_ERROR("[i=%d] ", i + 1);                                        \
    } else {                                                               \
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));\
    }                                                                      \
  }                                                                        \
                                                                           \
  GEOS_FINISH();                                                           \
  UNPROTECT(1);                                                            \
  return result;


SEXP geos_c_interpolate(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSInterpolate_r, double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_interpolate_normalized(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSInterpolateNormalized_r, double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_point_n(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSGeomGetPointN_r, int, INTEGER, paramPtr[i] == NA_INTEGER);
}

SEXP geos_c_simplify(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSSimplify_r, double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_simplify_preserve_topology(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSTopologyPreserveSimplify_r, double, REAL, ISNA(paramPtr[i]));
}

// set SRID modifies the input, so we need to clone first
SEXP geos_c_set_srid(SEXP geom, SEXP srid) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  int* pSrid = INTEGER(srid);

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue || pSrid[i] == NA_INTEGER) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    geometryResult = GEOSGeom_clone_r(handle, geometry);

    // don't know how to trigger this
    if (geometryResult == NULL) {
      UNPROTECT(1); // # nocov
      GEOS_ERROR("[i=%d] ", i + 1); // # nocov
    }

    // has no return code for exception
    GEOSSetSRID_r(handle, geometryResult, pSrid[i]);

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}

SEXP geos_c_minimum_bounding_circle(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;

  // not using the centre or radius for now, but these are needed for
  // call to GEOSMinimumBoundingCircle_r
  GEOSGeometry* center = GEOSGeom_createPointFromXY_r(handle, NAN, NAN);
  double radius;
  if (center == NULL) {
    GEOS_ERROR("Error allocating %s", "center point"); // # nocov
  }

  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    geometryResult = GEOSMinimumBoundingCircle_r(handle, geometry, &radius, &center);

    if (geometryResult == NULL) {
      GEOSGeom_destroy_r(handle, center);
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i + 1);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

  GEOSGeom_destroy_r(handle, center);
  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}

SEXP geos_c_clip_by_rect(SEXP geom, SEXP xmin, SEXP ymin, SEXP xmax, SEXP ymax) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  double* pXmin = REAL(xmin);
  double* pYmin = REAL(ymin);
  double* pXmax = REAL(xmax);
  double* pYmax = REAL(ymax);

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue || ISNA(pXmin[i]) || ISNA(pYmin[i]) || ISNA(pXmax[i]) || ISNA(pYmax[i])) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    geometryResult = GEOSClipByRect_r(handle, geometry, pXmin[i], pYmin[i], pXmax[i], pYmax[i]);

    if (geometryResult == NULL) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i + 1);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}


SEXP geos_c_delaunay_triangulation(SEXP geom, SEXP tolerace, SEXP edges) {
  double dTolerance = REAL(tolerace)[0];
  int iEdges = LOGICAL(edges)[0];

  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    geometryResult = GEOSDelaunayTriangulation_r(handle, geometry, dTolerance, iEdges);

    if (geometryResult == NULL) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i + 1);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}

SEXP geos_c_voronoi_diagram(SEXP geom, SEXP env, SEXP tolerace, SEXP edges) {
  double dTolerance = REAL(tolerace)[0];
  int iEdges = LOGICAL(edges)[0];

  GEOSGeometry* envGeometry = NULL;

  if (env != R_NilValue) {
    envGeometry = (GEOSGeometry*) R_ExternalPtrAddr(env);
    if (envGeometry == NULL) {
      Rf_error("`env` is not a valid external pointer");
    }
  }

  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    geometryResult = GEOSVoronoiDiagram_r(handle, geometry, envGeometry, dTolerance, iEdges);

    if (geometryResult == NULL) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i + 1);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}


// buffer and offset_curve

#define GEOS_BUFFER(_call)                                     \
  if (!Rf_inherits(params, "geos_buffer_params"))  {             \
    Rf_error("`params` must be created using geos_buffer_params()");\
  }                                                              \
                                                                 \
  double* pDistance = REAL(distance);                            \
                                                                 \
  int quadSegs = INTEGER(VECTOR_ELT(params, 0))[0];              \
  int endCapStyle = INTEGER(VECTOR_ELT(params, 1))[0];           \
  int joinStyle = INTEGER(VECTOR_ELT(params, 2))[0];             \
  double mitreLimit = REAL(VECTOR_ELT(params, 3))[0];            \
  int singleSided = LOGICAL(VECTOR_ELT(params, 4))[0];           \
                                                                 \
  GEOS_INIT();                                                   \
  GEOSBufferParams* bufferParams = GEOSBufferParams_create_r(handle);\
  if (GEOSBufferParams_setEndCapStyle_r(handle, bufferParams, endCapStyle) == 0) {\
    GEOSBufferParams_destroy_r(handle, bufferParams);            \
    GEOS_ERROR("%s: ", "end_cap_style");                         \
  }                                                              \
  if (GEOSBufferParams_setJoinStyle_r(handle, bufferParams, joinStyle) == 0) {\
    GEOSBufferParams_destroy_r(handle, bufferParams);            \
    GEOS_ERROR("%s: ", "join_style");                            \
  }                                                              \
                                                                 \
  GEOSBufferParams_setQuadrantSegments_r(handle, bufferParams, quadSegs);\
  GEOSBufferParams_setMitreLimit_r(handle, bufferParams, mitreLimit);\
  GEOSBufferParams_setSingleSided_r(handle, bufferParams, singleSided);\
                                                                 \
  R_xlen_t size = Rf_xlength(geom);                              \
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));           \
                                                                 \
  SEXP item;                                                     \
  GEOSGeometry* geometry;                                        \
  GEOSGeometry* geometryResult;                                  \
  for (R_xlen_t i = 0; i < size; i++) {                          \
    item = VECTOR_ELT(geom, i);                                  \
                                                                 \
    if (item == R_NilValue || ISNA(pDistance[i])) {              \
      SET_VECTOR_ELT(result, i, R_NilValue);                     \
      continue;                                                  \
    }                                                            \
                                                                 \
    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);          \
    GEOS_CHECK_GEOMETRY(geometry, i);                            \
                                                                 \
    geometryResult = _call;                                      \
                                                                 \
    if (geometryResult == NULL) {                                \
      UNPROTECT(1);                                              \
      GEOS_ERROR("[i=%d] ", i + 1);                              \
    } else {                                                     \
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));\
    }                                                            \
  }                                                              \
                                                                 \
  GEOS_FINISH();                                                 \
  UNPROTECT(1);                                                  \
  return result;


SEXP geos_c_buffer(SEXP geom, SEXP distance, SEXP params) {
  GEOS_BUFFER(GEOSBufferWithParams_r(handle, geometry, bufferParams, pDistance[i]));
}

SEXP geos_c_offset_curve(SEXP geom, SEXP distance, SEXP params) {
  GEOS_BUFFER(GEOSOffsetCurve_r(handle, geometry, pDistance[i], quadSegs, joinStyle, mitreLimit));
}
