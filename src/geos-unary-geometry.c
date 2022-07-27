
#include "libgeos.h"
#include "geos-common.h"
#include "wk-v1.h"
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
      Rf_error("[%d] %s", i + 1, globalErrorMessage);                            \
    } else {                                                   \
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));\
    }                                                          \
  }                                                            \
                                                               \
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

SEXP geos_c_line_merge(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSLineMerge_r);
}

SEXP geos_c_clone(SEXP geom) {
  GEOS_UNARY_GEOMETRY(GEOSGeom_clone_r);
}

SEXP geos_c_constrained_delaunay_triangulation(SEXP geom) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 10, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 10, 0)) {
    ERROR_OLD_LIBGEOS("GEOSConstrainedDelaunayTriangulation_r()", "3.10.0");
  }

  GEOS_UNARY_GEOMETRY(GEOSConstrainedDelaunayTriangulation_r);
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSConstrainedDelaunayTriangulation_r()", "3.10.0");
#endif
}

SEXP geos_c_line_merge_directed(SEXP geom) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 11, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 11, 0)) {
    ERROR_OLD_LIBGEOS("GEOSLineMergeDirected_r()", "3.11.0");
  }

  GEOS_UNARY_GEOMETRY(GEOSLineMergeDirected_r);
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSLineMergeDirected_r()", "3.11.0");
#endif
}

struct transform_callback_data {
  wk_trans_t* trans;
  double xyzm_in[4];
  double xyzm_out[4];
  R_xlen_t i;
};

static int transform_callback(double* x, double* y, void* userdata) {
  struct transform_callback_data* data = (struct transform_callback_data*)userdata;

  data->xyzm_in[0] = *x;
  data->xyzm_in[1] = *y;

  int result = data->trans->trans(
    data->i,
    data->xyzm_in,
    data->xyzm_out,
    data->trans->trans_data
  );

  *x = data->xyzm_out[0];
  *y = data->xyzm_out[1];

  return result == WK_CONTINUE;
}

SEXP geos_c_transform_xy(SEXP geom, SEXP trans_xptr) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 11, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 11, 0)) {
    ERROR_OLD_LIBGEOS("GEOSGeom_transformXY_r()", "3.11.0");
  }

  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  struct transform_callback_data data;
  data.trans = (wk_trans_t*)R_ExternalPtrAddr(trans_xptr);
  data.xyzm_in[2] = NA_REAL;
  data.xyzm_out[3] = NA_REAL;

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

    data.i = i;
    geometryResult = GEOSGeom_transformXY_r(handle, geometry, &transform_callback, &data);

    if (geometryResult == NULL) {
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    }

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
  }

  UNPROTECT(1);
  return result;
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSGeom_transformXY_r()", "3.11.0");
#endif
}

SEXP geos_c_make_valid_with_params(SEXP geom, SEXP params_sexp) {
  if (!Rf_inherits(params_sexp, "geos_make_valid_params")) {
    Rf_error("`make_valid_params` must be created using `geos_make_valid_params()`");
  }

  int keepCollapsed = LOGICAL(VECTOR_ELT(params_sexp, 0))[0];
  int method = INTEGER(VECTOR_ELT(params_sexp, 1))[0];

  // non-default options need GEOS 3.10 and GEOSMakeValidWithParams_r()
  // for now, just use the previous method
  if ((keepCollapsed == 1) && (method == 0)) {
    return geos_c_make_valid(geom);
  }

#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 10, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 10, 0)) {
    ERROR_OLD_LIBGEOS("GEOSMakeValidWithParams_r()", "3.10.0");
  }

  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  GEOSMakeValidParams* params = GEOSMakeValidParams_create_r(handle);
  if (params == NULL) {
    Rf_error("Failed to alloc GEOSMakeValidWithParams_r()"); // # nocov
  }

  if (GEOSMakeValidParams_setKeepCollapsed_r(handle, params, keepCollapsed) == 0) {
    GEOSMakeValidParams_destroy_r(handle, params); // # nocov
    GEOS_ERROR("%s: ", "keep_collapsed"); // # nocov
  }

  if (GEOSMakeValidParams_setMethod_r(handle, params, method) == 0) {
    GEOSMakeValidParams_destroy_r(handle, params);
    GEOS_ERROR("%s: ", "method"); // # nocov
  }

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

    geometryResult = GEOSMakeValidWithParams_r(handle, geometry, params);

    if (geometryResult == NULL) {
      GEOSMakeValidParams_destroy_r(handle, params);
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

  GEOSMakeValidParams_destroy_r(handle, params);
  UNPROTECT(1);
  return result;

#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSMakeValidWithParams_r()", "3.10.0");
#endif
}


#define GEOS_UNARY_GEOMETRY_PARAM(_call, _param_scalar, _param_ptr, _na_check)        \
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
    geometryResult = _call;                                                \
                                                                           \
    if (geometryResult == NULL) {                                          \
      Rf_error("[%d] %s", i + 1, globalErrorMessage);                                        \
    } else {                                                               \
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));\
    }                                                                      \
  }                                                                        \
                                                                           \
    UNPROTECT(1);                                                            \
  return result;


SEXP geos_c_interpolate(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSInterpolate_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_interpolate_normalized(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSInterpolateNormalized_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_point_n(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSGeomGetPointN_r(handle, geometry, paramPtr[i]), int, INTEGER, paramPtr[i] == NA_INTEGER);
}

SEXP geos_c_simplify(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSSimplify_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_simplify_preserve_topology(SEXP geom, SEXP param) {
  GEOS_UNARY_GEOMETRY_PARAM(GEOSTopologyPreserveSimplify_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_unary_union_prec(SEXP geom, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 9, 1)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 9, 1)) {
    ERROR_OLD_LIBGEOS("GEOSUnaryUnionPrec_r()", "3.9.1");
  }

  GEOS_UNARY_GEOMETRY_PARAM(GEOSUnaryUnionPrec_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSUnaryUnionPrec_r()", "3.9.1");
#endif
}

SEXP geos_c_maximum_inscribed_circle(SEXP geom, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 9, 1)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 9, 1)) {
    ERROR_OLD_LIBGEOS("GEOSMaximumInscribedCircle_r()", "3.9.1");
  }

  GEOS_UNARY_GEOMETRY_PARAM(GEOSMaximumInscribedCircle_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSMaximumInscribedCircle_r()", "3.9.1");
#endif
}

SEXP geos_c_densify(SEXP geom, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 10, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 10, 0)) {
    ERROR_OLD_LIBGEOS("GEOSDensify_r()", "3.10.0");
  }

  GEOS_UNARY_GEOMETRY_PARAM(GEOSDensify_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSDensify_r()", "3.10.0");
#endif
}

SEXP geos_c_remove_repeated_points(SEXP geom, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 11, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 11, 0)) {
    ERROR_OLD_LIBGEOS("GEOSRemoveRepeatedPoints_r()", "3.11.0");
  }

  GEOS_UNARY_GEOMETRY_PARAM(GEOSRemoveRepeatedPoints_r(handle, geometry, paramPtr[i]), double, REAL, ISNA(paramPtr[i]));
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSRemoveRepeatedPoints_r()", "3.11.0");
#endif
}

// this should really be defined in libgeos.h and probably will be in future versions
#ifndef GEOS_PREC_NO_TOPO
#define GEOS_PREC_NO_TOPO         (1<<0)
#define GEOS_PREC_KEEP_COLLAPSED  (1<<1)
#endif

SEXP geos_c_set_precision(SEXP geom, SEXP param, SEXP preserveTopology, SEXP keepCollapsed) {
  int flags = 0;
  if (!LOGICAL(preserveTopology)[0]) {
    flags = flags | GEOS_PREC_NO_TOPO;
  }

  if (LOGICAL(keepCollapsed)[0]) {
    flags = flags | GEOS_PREC_KEEP_COLLAPSED;
  }

  GEOS_UNARY_GEOMETRY_PARAM(GEOSGeom_setPrecision_r(handle, geometry, paramPtr[i], flags), double, REAL, ISNA(paramPtr[i]));
}

SEXP geos_c_concave_hull(SEXP geom, SEXP param, SEXP allowHoles_sexp) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 11, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 11, 0)) {
    ERROR_OLD_LIBGEOS("GEOSConcaveHull_r()", "3.11.0");
  }

  int allowHoles = INTEGER(allowHoles_sexp)[0];
  GEOS_UNARY_GEOMETRY_PARAM(GEOSConcaveHull_r(handle, geometry, paramPtr[i], allowHoles), double, REAL, ISNA(paramPtr[i]));
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSConcaveHull_r()", "3.11.0");
#endif
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
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    // has no return code for exception
    GEOSSetSRID_r(handle, geometryResult, pSrid[i]);

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
  }

    UNPROTECT(1);
  return result;
}

// Normalize modifies the input, so we need to clone first
// (but also has a return code, so can't be grouped with set srid)
SEXP geos_c_normalize(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;
  int returnCode;

  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    geometryResult = GEOSGeom_clone_r(handle, geometry);

    // don't know how to trigger this
    if (geometryResult == NULL) {
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    returnCode = GEOSNormalize_r(handle, geometryResult);

    if (returnCode == -1) {
      GEOSGeom_destroy_r(handle, geometryResult); // # nocov
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
  }

    UNPROTECT(1);
  return result;
}


SEXP geos_c_minimum_bounding_circle(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP radius = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP x = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP y = PROTECT(Rf_allocVector(REALSXP, size));

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;

  GEOSGeometry* center;
  double* pRadius = REAL(radius);
  double* pX = REAL(x);
  double* pY = REAL(y);

  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      pX[i] = NA_REAL;
      pY[i] = NA_REAL;
      pRadius[i] = NA_REAL;
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    geometryResult = GEOSMinimumBoundingCircle_r(handle, geometry, pRadius + i, &center);

    if (geometryResult == NULL) {
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
      GEOSGeomGetX_r(handle, center, pX + i);
      GEOSGeomGetY_r(handle, center, pY + i);
      GEOSGeom_destroy_r(handle, center);
    }
  }


  Rf_setAttrib(result, Rf_install("x"), x);
  Rf_setAttrib(result, Rf_install("y"), y);
  Rf_setAttrib(result, Rf_install("radius"), radius);
  UNPROTECT(4);
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
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

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
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

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
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
    }
  }

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
  if (bufferParams == NULL) {                                    \
    Rf_error("Failed to alloc GEOSBufferParams");                \
  }                                                              \
                                                                 \
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
    if (geometry == NULL) {                                      \
      GEOSBufferParams_destroy_r(handle, bufferParams);          \
      GEOS_CHECK_GEOMETRY(geometry, i);                          \
    }                                                            \
                                                                 \
    geometryResult = _call;                                      \
                                                                 \
    if (geometryResult == NULL) {                                \
      GEOSBufferParams_destroy_r(handle, bufferParams);          \
      Rf_error("[%d] %s", i + 1, globalErrorMessage);                              \
    } else {                                                     \
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));\
    }                                                            \
  }                                                              \
                                                                 \
  GEOSBufferParams_destroy_r(handle, bufferParams);              \
  UNPROTECT(1);                                                  \
  return result;


SEXP geos_c_buffer(SEXP geom, SEXP distance, SEXP params) {
  GEOS_BUFFER(GEOSBufferWithParams_r(handle, geometry, bufferParams, pDistance[i]));
}

SEXP geos_c_offset_curve(SEXP geom, SEXP distance, SEXP params) {
  GEOS_BUFFER(GEOSOffsetCurve_r(handle, geometry, pDistance[i], quadSegs, joinStyle, mitreLimit));
}


// access child geometries of a parent
SEXP geos_c_geometry_n(SEXP geom, SEXP n) {
  int* pN = INTEGER(n);

  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  const GEOSGeometry* geometryResult;
  int nGeoms;

  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue || pN[i] == NA_INTEGER) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    // extraction can result in segfault rather than exception here
    // so check indexes manually (use R-style NA for out-of-bounds
    // index)
    nGeoms = GEOSGetNumGeometries_r(handle, geometry);
    if (pN[i] < 0 || pN[i] >= nGeoms) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometryResult = GEOSGetGeometryN_r(handle, geometry, pN[i]);

    // don't know how to make this occur
    if (geometryResult == NULL) {
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    SET_VECTOR_ELT(result, i, geos_common_child_geometry_xptr(geometryResult, item));
  }

    UNPROTECT(1);
  return result;
}

// access child rings of a parent
SEXP geos_c_ring_n(SEXP geom, SEXP n) {
  int* pN = INTEGER(n);

  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  const GEOSGeometry* geometryResult;
  int nGeoms;

  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue || pN[i] == NA_INTEGER) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    // error for non-polygons
    if (GEOSGeomTypeId_r(handle, geometry) != GEOS_POLYGON) {
            Rf_error("[i=%d] Can't extract rings from a non-polygon", i);
    }

    // extraction can result in segfault rather than exception here
    // so check indexes manually (use R-style NA for out-of-bounds
    // index)
    nGeoms = GEOSGetNumInteriorRings_r(handle, geometry) + 1;
    if (pN[i] < 0 || pN[i] >= nGeoms) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    // do a slightly modified interpretation of ring "n" such that
    // the outer ring is always the 0th ring
    if (pN[i] == 0) {
      geometryResult = GEOSGetExteriorRing_r(handle, geometry);
    } else {
      geometryResult = GEOSGetInteriorRingN_r(handle, geometry, pN[i] - 1);
    }

    // don't know how to make this occur
    if (geometryResult == NULL) {
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    SET_VECTOR_ELT(result, i, geos_common_child_geometry_xptr(geometryResult, item));
  }

    UNPROTECT(1);
  return result;
}
