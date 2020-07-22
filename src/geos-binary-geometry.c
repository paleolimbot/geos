
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

SEXP geos_c_snap(SEXP geom1, SEXP geom2, SEXP tolerance) {
  R_xlen_t size = Rf_xlength(geom1);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  double* pTolerance = REAL(tolerance);

  GEOS_INIT();

  SEXP item1;
  SEXP item2;
  GEOSGeometry* geometry1;
  GEOSGeometry* geometry2;
  GEOSGeometry* geometryResult;
  for (R_xlen_t i = 0; i < size; i++) {
    item1 = VECTOR_ELT(geom1, i);
    item2 = VECTOR_ELT(geom2, i);

    if (item1 == R_NilValue || item2 == R_NilValue || ISNA(pTolerance[i])) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);
    GEOS_CHECK_GEOMETRY(geometry1, i);
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);
    GEOS_CHECK_GEOMETRY(geometry2, i);

    geometryResult = GEOSSnap_r(handle, geometry1, geometry2, pTolerance[i]);

    if (geometryResult == NULL) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i + 1);
    }

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometryResult));
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}
