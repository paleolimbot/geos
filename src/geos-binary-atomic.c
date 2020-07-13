
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

#define GEOS_BINARY_REAL(_func, _errorValue)                      \
  R_xlen_t size = Rf_xlength(geom1);                              \
  SEXP result = PROTECT(Rf_allocVector(REALSXP, size));           \
  double* pResult = REAL(result);                                 \
                                                                  \
  GEOS_INIT();                                                    \
                                                                  \
  SEXP item1;                                                     \
  SEXP item2;                                                     \
  GEOSGeometry* geometry1;                                        \
  GEOSGeometry* geometry2;                                        \
  for (R_xlen_t i = 0; i < size; i++) {                           \
    item1 = VECTOR_ELT(geom1, i);                                 \
    item2 = VECTOR_ELT(geom2, i);                                 \
                                                                  \
    if (item1 == R_NilValue || item2 == R_NilValue) {             \
      pResult[i] = NA_REAL;                                       \
      continue;                                                   \
    }                                                             \
                                                                  \
    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);         \
    GEOS_CHECK_GEOMETRY(geometry1, i);                            \
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);         \
    GEOS_CHECK_GEOMETRY(geometry2, i);                            \
                                                                  \
    int resultCode = _func(handle, geometry1, geometry2, &pResult[i]);        \
                                                                  \
    if (resultCode == _errorValue) {                              \
      UNPROTECT(1);                                               \
      GEOS_ERROR("[i=%d] ", i + 1);                               \
    }                                                             \
  }                                                               \
                                                                  \
  GEOS_FINISH();                                                  \
  UNPROTECT(1);                                                   \
  return result;


SEXP geos_c_distance(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_REAL(GEOSDistance_r, 0);
}

SEXP geos_c_distance_indexed(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_REAL(GEOSDistanceIndexed_r, 0);
}

SEXP geos_c_distance_hausdorff(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_REAL(GEOSHausdorffDistance_r, 0);
}

SEXP geos_c_distance_frechet(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_REAL(GEOSFrechetDistance_r, 0);
}


#define GEOS_BINARY_PREDICATE(_func) \
  R_xlen_t size = Rf_xlength(geom1);                                          \
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));                        \
  int* pResult = INTEGER(result);                                             \
                                                                              \
  GEOS_INIT();                                                                \
                                                                              \
  SEXP item1;                                                                 \
  SEXP item2;                                                                 \
  GEOSGeometry* geometry1;                                                    \
  GEOSGeometry* geometry2;                                                    \
  for (R_xlen_t i = 0; i < size; i++) {                                       \
    item1 = VECTOR_ELT(geom1, i);                                             \
    item2 = VECTOR_ELT(geom2, i);                                             \
                                                                              \
    if (item1 == R_NilValue || item2 == R_NilValue) {                         \
      pResult[i] = NA_LOGICAL;                                                \
      continue;                                                               \
    }                                                                         \
                                                                              \
    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);                     \
    GEOS_CHECK_GEOMETRY(geometry1, i);                                        \
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);                     \
    GEOS_CHECK_GEOMETRY(geometry2, i);                                        \
                                                                              \
    int resultCode = _func(handle, geometry1, geometry2);                     \
                                                                              \
    if (resultCode == 2) {                                                    \
      UNPROTECT(1);                                                           \
      GEOS_ERROR("[i=%d] ", i + 1);                                           \
    }                                                                         \
    pResult[i] = resultCode;                                                  \
  }                                                                           \
                                                                              \
  GEOS_FINISH();                                                              \
  UNPROTECT(1);                                                               \
  return result;


SEXP geos_c_disjoint(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSDisjoint_r);
}

SEXP geos_c_touches(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSTouches_r);
}

SEXP geos_c_intersects(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSIntersects_r);
}

SEXP geos_c_crosses(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSCrosses_r);
}

SEXP geos_c_within(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSWithin_r);
}

SEXP geos_c_contains(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSContains_r);
}

SEXP geos_c_overlaps(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSOverlaps_r);
}

SEXP geos_c_equals(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSEquals_r);
}

SEXP geos_c_covers(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSCovers_r);
}

SEXP geos_c_covered_by(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_PREDICATE(GEOSCoveredBy_r);
}

// equals exact is the odd one out here because it takes a single parameter
SEXP geos_c_equals_exact(SEXP geom1, SEXP geom2, SEXP tolerance) {
  R_xlen_t size = Rf_xlength(geom1);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);
  double* pTolerance = REAL(tolerance);

  GEOS_INIT();

  SEXP item1;
  SEXP item2;
  GEOSGeometry* geometry1;
  GEOSGeometry* geometry2;
  for (R_xlen_t i = 0; i < size; i++) {
    item1 = VECTOR_ELT(geom1, i);
    item2 = VECTOR_ELT(geom2, i);

    if (item1 == R_NilValue || item2 == R_NilValue || ISNA(pTolerance[i])) {
      pResult[i] = NA_REAL;
      continue;
    }

    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);
    GEOS_CHECK_GEOMETRY(geometry1, i);
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);
    GEOS_CHECK_GEOMETRY(geometry2, i);

    int resultCode = GEOSEqualsExact_r(handle, geometry1, geometry2, pTolerance[i]);

    if (resultCode == 2) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i + 1);
    }

    pResult[i] = resultCode;
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}
