
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


#define GEOS_UNARY_GEOMETRY_PARAM(_func, _param_scalar, _param_ptr)        \
  R_xlen_t size = Rf_xlength(geom);                                        \
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));                     \
  _param_scalar* paramPtr = _param_ptr(param);                                    \
  GEOS_INIT();                                                             \
                                                                           \
  SEXP item;                                                               \
  GEOSGeometry* geometry;                                                  \
  GEOSGeometry* geometryResult;                                            \
  for (R_xlen_t i = 0; i < size; i++) {                                    \
    item = VECTOR_ELT(geom, i);                                            \
                                                                           \
    if (item == R_NilValue) {                                              \
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
  GEOS_UNARY_GEOMETRY_PARAM(GEOSInterpolate_r, double, REAL);
}

SEXP geos_c_interpolate_normalized(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSInterpolateNormalized_r, double, REAL);
}

SEXP geos_c_point_n(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSGeomGetPointN_r, int, INTEGER);
}

SEXP geos_c_simplify(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSSimplify_r, double, REAL);
}

SEXP geos_c_simplify_preserve_topology(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSTopologyPreserveSimplify_r, double, REAL);
}
