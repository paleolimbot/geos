
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

#define GEOS_BINARY(_func)                                                  \
  R_xlen_t size = Rf_xlength(geom1);                                          \
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));                        \
                                                                              \
  GEOS_INIT();                                                                \
                                                                              \
  SEXP item1;                                                                 \
  SEXP item2;                                                                 \
  GEOSGeometry* geometry1;                                                    \
  GEOSGeometry* geometry2;                                                    \
  GEOSGeometry* geometryResult;                                               \
  for (R_xlen_t i = 0; i < size; i++) {                                       \
    item1 = VECTOR_ELT(geom1, i);                                             \
    item2 = VECTOR_ELT(geom2, i);                                             \
                                                                              \
    if (item1 == R_NilValue || item2 == R_NilValue) {                         \
      SET_VECTOR_ELT(result, i, R_NilValue);                                  \
      continue;                                                               \
    }                                                                         \
                                                                              \
    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);                     \
    GEOS_CHECK_GEOMETRY(geometry1, i);                                        \
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);                     \
    GEOS_CHECK_GEOMETRY(geometry2, i);                                        \
                                                                              \
    geometryResult = _func(handle, geometry1, geometry2);                     \
                                                                              \
    if (geometryResult == NULL) {                                             \
      UNPROTECT(1);                                                           \
      GEOS_ERROR("[i=%d] ", i + 1);                                           \
    }                                                                         \
    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));     \
  }                                                                           \
                                                                              \
  GEOS_FINISH();                                                              \
  UNPROTECT(1);                                                               \
  return result;


SEXP geos_c_intersection(SEXP geom1, SEXP geom2) {
  GEOS_BINARY(GEOSIntersection_r);
}

SEXP geos_c_difference(SEXP geom1, SEXP geom2) {
  GEOS_BINARY(GEOSDifference_r);
}

SEXP geos_c_sym_difference(SEXP geom1, SEXP geom2) {
  GEOS_BINARY(GEOSSymDifference_r);
}

SEXP geos_c_union(SEXP geom1, SEXP geom2) {
  GEOS_BINARY(GEOSUnion_r);
}

SEXP geos_c_shared_paths(SEXP geom1, SEXP geom2) {
  GEOS_BINARY(GEOSSharedPaths_r);
}

#define GEOS_BINARY_REAL(_func)                           \
R_xlen_t size = Rf_xlength(geom1);                        \
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));    \
  double* pParam = REAL(param);                   \
                                                          \
  GEOS_INIT();                                            \
                                                          \
  SEXP item1;                                             \
  SEXP item2;                                             \
  GEOSGeometry* geometry1;                                \
  GEOSGeometry* geometry2;                                \
  GEOSGeometry* geometryResult;                           \
  for (R_xlen_t i = 0; i < size; i++) {                   \
    item1 = VECTOR_ELT(geom1, i);                         \
    item2 = VECTOR_ELT(geom2, i);                         \
                                                          \
    if (item1 == R_NilValue || item2 == R_NilValue || ISNA(pParam[i])) {  \
      SET_VECTOR_ELT(result, i, R_NilValue);              \
      continue;                                           \
    }                                                     \
                                                          \
    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1); \
    GEOS_CHECK_GEOMETRY(geometry1, i);                    \
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2); \
    GEOS_CHECK_GEOMETRY(geometry2, i);                    \
                                                          \
    geometryResult = _func(handle, geometry1, geometry2, pParam[i]); \
                                                          \
    if (geometryResult == NULL) {                         \
      UNPROTECT(1);                                       \
      GEOS_ERROR("[i=%d] ", i + 1);                       \
    }                                                     \
                                                          \
    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult)); \
  }                                                       \
                                                          \
  GEOS_FINISH();                                          \
  UNPROTECT(1);                                           \
  return result;

SEXP geos_c_snap(SEXP geom1, SEXP geom2, SEXP param) {
  GEOS_BINARY_REAL(GEOSSnap_r);
}

// *Prec_r() binary operators were added in libgeos 3.9.1-1
SEXP geos_c_intersection_prec(SEXP geom1, SEXP geom2, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 9, 1)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 9, 1)) {
    ERROR_OLD_LIBGEOS("GEOSIntersectionPrec_r()", "3.9.1");
  }

  GEOS_BINARY_REAL(GEOSIntersectionPrec_r);
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSIntersectionPrec_r()", "3.9.1");
#endif
}

SEXP geos_c_difference_prec(SEXP geom1, SEXP geom2, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 9, 1)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 9, 1)) {
    ERROR_OLD_LIBGEOS("GEOSDifferencePrec_r()", "3.9.1");
  }

  GEOS_BINARY_REAL(GEOSDifferencePrec_r);
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSDifferencePrec_r()", "3.9.1");
#endif
}

SEXP geos_c_sym_difference_prec(SEXP geom1, SEXP geom2, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 9, 1)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 9, 1)) {
    ERROR_OLD_LIBGEOS("GEOSSymDifferencePrec_r()", "3.9.1");
  }

  GEOS_BINARY_REAL(GEOSSymDifferencePrec_r);
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSSymDifferencePrec_r()", "3.9.1");
#endif
}

SEXP geos_c_union_prec(SEXP geom1, SEXP geom2, SEXP param) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 9, 1)
  if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 9, 1)) {
    ERROR_OLD_LIBGEOS("GEOSUnionPrec_r()", "3.9.1");
  }

  GEOS_BINARY_REAL(GEOSUnionPrec_r);
#else
  ERROR_OLD_LIBGEOS_BUILD("GEOSUnionPrec_r()", "3.9.1");
#endif
}

SEXP geos_c_clearance_line_between(SEXP geom1, SEXP geom2) {
  R_xlen_t size = Rf_xlength(geom1);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  SEXP item1;
  SEXP item2;
  GEOSGeometry* geometry1;
  GEOSGeometry* geometry2;
  GEOSCoordSequence* sequenceResult;
  GEOSGeometry* geometryResult;

  for (R_xlen_t i = 0; i < size; i++) {
    item1 = VECTOR_ELT(geom1, i);
    item2 = VECTOR_ELT(geom2, i);

    if (item1 == R_NilValue || item2 == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);
    GEOS_CHECK_GEOMETRY(geometry1, i);
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);
    GEOS_CHECK_GEOMETRY(geometry2, i);

    // if either is EMPTY we return EMPTY here, because an unknown
    // error occurs otherwise
    if (GEOSisEmpty_r(handle, geometry1) || GEOSisEmpty_r(handle, geometry2)) {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(GEOSGeom_createEmptyLineString_r(handle)));
      continue;
    }

    sequenceResult = GEOSNearestPoints_r(handle, geometry1, geometry2);
    if (sequenceResult == NULL) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i + 1);
    }

    geometryResult = GEOSGeom_createLineString_r(handle, sequenceResult);

    // don't know how to make this fire
    if (geometryResult == NULL) {
      UNPROTECT(1); // # nocov
      GEOSCoordSeq_destroy_r(handle, sequenceResult); // # nocov
      GEOS_ERROR("[i=%d] ", i + 1); // # nocov
    }

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}
