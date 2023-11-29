
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

// These functions are in the form _func(handle, geometry, double*)
// and return 0 on exception (except GEOSMinimumClearance_r, which
// returns 2 on exception)
#define GEOS_UNARY_REAL(_func, _errorValue)                     \
  R_xlen_t size = Rf_xlength(geom);                            \
  SEXP result = PROTECT(Rf_allocVector(REALSXP, size));        \
  double* pResult = REAL(result);                              \
                                                               \
  GEOS_INIT();                                                 \
                                                               \
  SEXP item;                                                   \
  GEOSGeometry* geometry;                                      \
  for (R_xlen_t i = 0; i < size; i++) {                        \
    item = VECTOR_ELT(geom, i);                                \
                                                               \
    if (item == R_NilValue) {                                  \
      pResult[i] = NA_REAL;                                    \
      continue;                                                \
    }                                                          \
                                                               \
    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);        \
    GEOS_CHECK_GEOMETRY(geometry, i);                          \
                                                               \
    int resultCode = _func(handle, geometry, &pResult[i]);     \
                                                               \
    if (resultCode == _errorValue) {                           \
      Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);                            \
    }                                                          \
  }                                                            \
                                                               \
    UNPROTECT(1);                                                \
  return result;


SEXP geos_c_area(SEXP geom) {
  GEOS_UNARY_REAL(GEOSArea_r, 0);
}

SEXP geos_c_length(SEXP geom) {
  GEOS_UNARY_REAL(GEOSLength_r, 0);
}

SEXP geos_c_x(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetX_r, 0);
}

SEXP geos_c_y(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetY_r, 0);
}

SEXP geos_c_z(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetZ_r, 0);
}

SEXP geos_c_xmin(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getXMin_r, 0);
}

SEXP geos_c_ymin(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getYMin_r, 0);
}

SEXP geos_c_xmax(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getXMax_r, 0);
}

SEXP geos_c_ymax(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getYMax_r, 0);
}

SEXP geos_c_minimum_clearance(SEXP geom) {
  GEOS_UNARY_REAL(GEOSMinimumClearance_r, 2);
}

SEXP geos_c_extent(SEXP geom) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 11, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 11, 0)) {
    ERROR_OLD_LIBGEOS("GEOSGeom_getExtent_r()", "3.11.0");
  }

  R_xlen_t size = Rf_xlength(geom);

  SEXP xmin_sexp = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP ymin_sexp = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP xmax_sexp = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP ymax_sexp = PROTECT(Rf_allocVector(REALSXP, size));
  double* xmin = REAL(xmin_sexp);
  double* ymin = REAL(ymin_sexp);
  double* xmax = REAL(xmax_sexp);
  double* ymax = REAL(ymax_sexp);

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      xmin[i] = NA_REAL;
      ymin[i] = NA_REAL;
      xmax[i] = NA_REAL;
      ymax[i] = NA_REAL;
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    if (GEOSisEmpty_r(handle, geometry)) {
      xmin[i] = R_PosInf;
      ymin[i] = R_PosInf;
      xmax[i] = R_NegInf;
      ymax[i] = R_NegInf;
      continue;
    }

    int resultCode = GEOSGeom_getExtent_r(
      handle,
      geometry,
      xmin + i,
      ymin + i,
      xmax + i,
      ymax + i
    );

    // This gets fired on EMPTY, but we catch this case above
    if (resultCode == 0) {
      Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);
    }
  }

  const char* names[] = {"xmin", "ymin", "xmax", "ymax", ""};
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
  SET_VECTOR_ELT(result, 0, xmin_sexp);
  SET_VECTOR_ELT(result, 1, ymin_sexp);
  SET_VECTOR_ELT(result, 2, xmax_sexp);
  SET_VECTOR_ELT(result, 3, ymax_sexp);

  UNPROTECT(5);
  return result;
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSGeom_getExtent_r()", "3.11.0");
#endif
}

SEXP geos_c_hilbert_code(SEXP geom, SEXP geomExtent, SEXP level_sexp) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 11, 0)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 11, 0)) {
    ERROR_OLD_LIBGEOS("GEOSHilbertCode_r()", "3.11.0");
  }

  GEOSGeometry* extent = (GEOSGeometry*) R_ExternalPtrAddr(VECTOR_ELT(geomExtent, 0));
  GEOS_CHECK_GEOMETRY(extent, 0);

  int level = INTEGER(level_sexp)[0];
  R_xlen_t size = Rf_xlength(geom);
  SEXP code_sexp = PROTECT(Rf_allocVector(INTSXP, size));
  int* code = INTEGER(code_sexp);

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  unsigned int item_code;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      code[i] = NA_INTEGER;
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    int resultCode = GEOSHilbertCode_r(
      handle,
      geometry,
      extent,
      level,
      &item_code
    );

    if (resultCode != 1) {
      Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);
    }

    code[i] = item_code;
  }

  UNPROTECT(1);
  return code_sexp;
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSHilbertCode_r()", "3.11.0");
#endif
}

// These functions are in the form _scalar_type _func(handle, geometry)
// and return a variety of values on exception
#define GEOS_UNARY_RETURN(_func, _scalar_type, _vec_type, _vec_ptr, _na_value, _errorValue)  \
  R_xlen_t size = Rf_xlength(geom);                               \
  SEXP result = PROTECT(Rf_allocVector(_vec_type, size));         \
  _scalar_type* pResult = _vec_ptr(result);                       \
                                                                  \
  GEOS_INIT();                                                    \
                                                                  \
  SEXP item;                                                      \
  GEOSGeometry* geometry;                                         \
  for (R_xlen_t i = 0; i < size; i++) {                           \
    item = VECTOR_ELT(geom, i);                                   \
                                                                  \
    if (item == R_NilValue) {                                     \
      pResult[i] = _na_value;                                     \
      continue;                                                   \
    }                                                             \
                                                                  \
    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);           \
    GEOS_CHECK_GEOMETRY(geometry, i);                             \
                                                                  \
    int resultCode = _func(handle, geometry);                     \
                                                                  \
    if (resultCode == _errorValue) {                              \
      Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);                               \
    } else {                                                      \
      pResult[i] = resultCode;                                    \
    }                                                             \
  }                                                               \
                                                                  \
    UNPROTECT(1);                                                   \
  return result;


SEXP geos_c_is_empty(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSisEmpty_r, int, LGLSXP, LOGICAL, NA_LOGICAL, 2);
}

SEXP geos_c_is_simple(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSisSimple_r, int, LGLSXP, LOGICAL, NA_LOGICAL, 2);
}

SEXP geos_c_is_ring(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSisRing_r, int, LGLSXP, LOGICAL, NA_LOGICAL, 2);
}

SEXP geos_c_has_z(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSHasZ_r, int, LGLSXP, LOGICAL, NA_LOGICAL, 2);
}

SEXP geos_c_is_closed(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSisClosed_r, int, LGLSXP, LOGICAL, NA_LOGICAL, 2);
}

SEXP geos_c_type_id(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSGeomTypeId_r, int, INTSXP, INTEGER, NA_INTEGER, -1);
}

SEXP geos_c_precision(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSGeom_getPrecision_r, double, REALSXP, REAL, NA_REAL, -1);
}

SEXP geos_c_srid(SEXP geom) {
  // return 0 on exception, but this is also the SRID of 'unset'
  GEOS_UNARY_RETURN(GEOSGetSRID_r, int, INTSXP, INTEGER, NA_INTEGER, -1);
}

SEXP geos_c_num_coordinates(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSGetNumCoordinates_r, int, INTSXP, INTEGER, NA_INTEGER, -1);
}

SEXP geos_c_num_geometries(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSGetNumGeometries_r, int, INTSXP, INTEGER, NA_INTEGER, -1);
}

SEXP geos_c_num_interior_rings(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSGetNumInteriorRings_r, int, INTSXP, INTEGER, NA_INTEGER, -1);
}

SEXP geos_c_dimension(SEXP geom) {
  // docs say returns 0 on exception, but also for EMPTY
  // but returns -1 for GEOMETRYCOLLECTION EMPTY
  GEOS_UNARY_RETURN(GEOSGeom_getDimensions_r, int, INTSXP, INTEGER, NA_INTEGER, -1);
}

SEXP geos_c_coorinate_dimension(SEXP geom) {
  // unclear what it would return on exception
  GEOS_UNARY_RETURN(GEOSGeom_getCoordinateDimension_r, int, INTSXP, INTEGER, NA_INTEGER, 0);
}

// GEOSCoordSeq_isCCW() is useful but operates on coordinate sequence instead of geom
SEXP geos_c_is_clockwise(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  const GEOSCoordSequence* seq;
  char isCCW;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      pResult[i] = NA_LOGICAL;
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    if (GEOSisEmpty_r(handle, geometry)) {
      pResult[i] = NA_LOGICAL;
      continue;
    }

    seq = GEOSGeom_getCoordSeq_r(handle, geometry);
    // e.g., when not a point, linestring, or linearring
    if (seq == NULL) {
      Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);
    }

    int resultCode = GEOSCoordSeq_isCCW_r(handle, seq, &isCCW);

    // e.g., not enough points in ring
    if (resultCode == 0) {
      Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);
    }

    pResult[i] = !isCCW;
  }

    UNPROTECT(1);
  return result;
}

// validity checking

SEXP geos_c_is_valid(SEXP geom) {
  GEOS_UNARY_RETURN(GEOSisValid_r, int, LGLSXP, LOGICAL, NA_LOGICAL, 2);
}

SEXP geos_c_is_valid_detail(SEXP geom, SEXP allowSelfTouchingRingFormingHole) {
  int flags = LOGICAL(allowSelfTouchingRingFormingHole)[0];

  R_xlen_t size = Rf_xlength(geom);
  SEXP resultIsValid = PROTECT(Rf_allocVector(LGLSXP, size));
  SEXP resultReason = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP resultLocation = PROTECT(Rf_allocVector(VECSXP, size));
  int* pResultIsValid = LOGICAL(resultIsValid);

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  GEOSGeometry* geometryResult;
  char* reasonResult;
  int validResult;

  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      pResultIsValid[i] = NA_LOGICAL;
      SET_STRING_ELT(resultReason, i, NA_STRING);
      SET_VECTOR_ELT(resultLocation, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    validResult = GEOSisValidDetail_r(handle, geometry, flags, &reasonResult, &geometryResult);

    // when valid or on  exception, both reasonResult and geometyResult are not set
    // (and therefore will segfault if one tries to destroy them)
    // returning GEOMETRYCOLLECTION EMPTY instead of NULL when there is no
    // error
    // (don't know how to trigger this error)
    if (validResult == 2) {
      Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage); // # nocov
    } else if (validResult == 1) {
      pResultIsValid[i] = 1;
      SET_STRING_ELT(resultReason, i, NA_STRING);
      SET_VECTOR_ELT(
        resultLocation,
        i,
        geos_common_geometry_xptr(GEOSGeom_createEmptyCollection_r(handle, GEOS_GEOMETRYCOLLECTION))
      );
    } else {
      pResultIsValid[i] = 0;
      SET_STRING_ELT(resultReason, i, Rf_mkChar(reasonResult));
      GEOSFree_r(handle, reasonResult);
      SET_VECTOR_ELT(resultLocation, i, geos_common_geometry_xptr(geometryResult));
    }
  }


  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(result, 0, resultIsValid);
  SET_VECTOR_ELT(result, 1, resultReason);
  SET_VECTOR_ELT(result, 2, resultLocation);
  UNPROTECT(4);
  return result;
}
