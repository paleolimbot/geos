
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

#define GEOS_DIST(_func)                      \
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
    if (resultCode == 0) {                                        \
      Rf_error("[%d] %s", i + 1, globalErrorMessage);             \
    }                                                             \
  }                                                               \
                                                                  \
    UNPROTECT(1);                                                   \
  return result;


SEXP geos_c_distance(SEXP geom1, SEXP geom2) {
  GEOS_DIST(GEOSDistance_r);
}

SEXP geos_c_distance_indexed(SEXP geom1, SEXP geom2) {
  GEOS_DIST(GEOSDistanceIndexed_r);
}

SEXP geos_c_distance_hausdorff(SEXP geom1, SEXP geom2) {
  GEOS_DIST(GEOSHausdorffDistance_r);
}

SEXP geos_c_distance_frechet(SEXP geom1, SEXP geom2) {
  GEOS_DIST(GEOSFrechetDistance_r);
}

#define GEOS_DIST_DENSIFY(_func)                                \
  R_xlen_t size = Rf_xlength(geom1);                                          \
  SEXP result = PROTECT(Rf_allocVector(REALSXP, size));                       \
  double* pResult = REAL(result);                                             \
  double densifyFracDouble = REAL(densifyFrac)[0];                            \
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
      pResult[i] = NA_REAL;                                                   \
      continue;                                                               \
    }                                                                         \
                                                                              \
    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);                     \
    GEOS_CHECK_GEOMETRY(geometry1, i);                                        \
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);                     \
    GEOS_CHECK_GEOMETRY(geometry2, i);                                        \
                                                                              \
    int resultCode = _func(handle, geometry1, geometry2, densifyFracDouble, &pResult[i]);  \
                                                                              \
    if (resultCode == 0) {                                          \
      Rf_error("[%d] %s", i + 1, globalErrorMessage);                                           \
    }                                                                         \
  }                                                                           \
                                                                              \
    UNPROTECT(1);                                                               \
  return result;

SEXP geos_c_distance_hausdorff_densify(SEXP geom1, SEXP geom2, SEXP densifyFrac) {
  GEOS_DIST_DENSIFY(GEOSHausdorffDistanceDensify_r);
}

SEXP geos_c_distance_frechet_densify(SEXP geom1, SEXP geom2, SEXP densifyFrac) {
  GEOS_DIST_DENSIFY(GEOSFrechetDistanceDensify_r);
}

// project and project _normalized both return their result rather than use a
// pointer arg (-1 for error, but this is undocumented)
#define GEOS_BINARY_REAL_RETURN(_func)                                        \
  R_xlen_t size = Rf_xlength(geom1);                                          \
  SEXP result = PROTECT(Rf_allocVector(REALSXP, size));                       \
  double* pResult = REAL(result);                                             \
                                                                              \
  GEOS_INIT();                                                                \
                                                                              \
  SEXP item1;                                                                 \
  SEXP item2;                                                                 \
  GEOSGeometry* geometry1;                                                    \
  GEOSGeometry* geometry2;                                                    \
  double itemResult;                                                          \
  for (R_xlen_t i = 0; i < size; i++) {                                       \
    item1 = VECTOR_ELT(geom1, i);                                             \
    item2 = VECTOR_ELT(geom2, i);                                             \
                                                                              \
    if (item1 == R_NilValue || item2 == R_NilValue) {                         \
      pResult[i] = NA_REAL;                                                   \
      continue;                                                               \
    }                                                                         \
                                                                              \
    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);                     \
    GEOS_CHECK_GEOMETRY(geometry1, i);                                        \
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);                     \
    GEOS_CHECK_GEOMETRY(geometry2, i);                                        \
                                                                              \
    itemResult = _func(handle, geometry1, geometry2);                         \
    if (itemResult == -1) {                                                   \
      Rf_error("[%d] %s", i + 1, globalErrorMessage);                                           \
    }                                                                         \
                                                                              \
    pResult[i] = itemResult;                                                  \
  }                                                                           \
                                                                              \
    UNPROTECT(1);                                                               \
  return result;

SEXP geos_c_project(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_REAL_RETURN(GEOSProject_r);
}

SEXP geos_c_project_normalized(SEXP geom1, SEXP geom2) {
  GEOS_BINARY_REAL_RETURN(GEOSProjectNormalized_r);
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
      Rf_error("[%d] %s", i + 1, globalErrorMessage);                                           \
    }                                                                         \
    pResult[i] = resultCode;                                                  \
  }                                                                           \
                                                                              \
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
      pResult[i] = NA_INTEGER;
      continue;
    }

    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);
    GEOS_CHECK_GEOMETRY(geometry1, i);
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);
    GEOS_CHECK_GEOMETRY(geometry2, i);

    int resultCode = GEOSEqualsExact_r(handle, geometry1, geometry2, pTolerance[i]);

    // don't know how to make this fire
    if (resultCode == 2) {
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    pResult[i] = resultCode;
  }

    UNPROTECT(1);
  return result;
}


#define GEOS_PREPARED_BINARY_PREDICATE(_func)                                        \
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
  const GEOSPreparedGeometry* prepared = geos_common_geometry_prepared(item1);  \
  if (prepared == NULL) {                                                   \
    Rf_error("[%d] %s", i + 1, globalErrorMessage);                         \
  }                                                                         \
  int resultCode = _func(handle, prepared, geometry2);                      \
  if (resultCode == 2) {                                                    \
    Rf_error("[%d] %s", i + 1, globalErrorMessage);                         \
  }                                                                         \
  pResult[i] = resultCode;                                                  \
}                                                                           \
                                                                            \
UNPROTECT(1);                                                               \
return result;

SEXP geos_c_prepared_disjoint(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedDisjoint_r);
}

SEXP geos_c_prepared_touches(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedTouches_r);
}

SEXP geos_c_prepared_intersects(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedIntersects_r);
}

SEXP geos_c_prepared_crosses(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedCrosses_r);
}

SEXP geos_c_prepared_within(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedWithin_r);
}

SEXP geos_c_prepared_contains(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedContains_r);
}

SEXP geos_c_prepared_contains_properly(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedContainsProperly_r);
}

SEXP geos_c_prepared_overlaps(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedOverlaps_r);
}

SEXP geos_c_prepared_covers(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedCovers_r);
}

SEXP geos_c_prepared_covered_by(SEXP geom1, SEXP geom2) {
  GEOS_PREPARED_BINARY_PREDICATE(GEOSPreparedCoveredBy_r);
}

// DE9IM relationships

SEXP geos_c_relate(SEXP geom1, SEXP geom2, SEXP boundaryNodeRule) {
  R_xlen_t size = Rf_xlength(geom1);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));
  int bnr = INTEGER(boundaryNodeRule)[0];

  GEOS_INIT();

  SEXP item1;
  SEXP item2;
  GEOSGeometry* geometry1;
  GEOSGeometry* geometry2;
  char* itemResult;
  for (R_xlen_t i = 0; i < size; i++) {
    item1 = VECTOR_ELT(geom1, i);
    item2 = VECTOR_ELT(geom2, i);

    if (item1 == R_NilValue || item2 == R_NilValue) {
      SET_STRING_ELT(result, i, NA_STRING);
      continue;
    }

    geometry1 = (GEOSGeometry*) R_ExternalPtrAddr(item1);
    GEOS_CHECK_GEOMETRY(geometry1, i);
    geometry2 = (GEOSGeometry*) R_ExternalPtrAddr(item2);
    GEOS_CHECK_GEOMETRY(geometry2, i);

    itemResult = GEOSRelateBoundaryNodeRule_r(handle, geometry1, geometry2, bnr);

    // don't know how to make this fire
    if (itemResult == NULL) {
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    SET_STRING_ELT(result, i, Rf_mkChar(itemResult));
    GEOSFree_r(handle, itemResult);
  }

    UNPROTECT(1);
  return result;
}

SEXP geos_c_relate_pattern_match(SEXP match, SEXP pattern) {
  R_xlen_t size = Rf_xlength(match);
  SEXP result =  PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);

  GEOS_INIT();

  int itemResult;
  SEXP itemMatch;
  SEXP itemPattern;
  for (R_xlen_t i = 0; i < size; i++) {
    itemMatch = STRING_ELT(match, i);
    itemPattern = STRING_ELT(pattern, i);
    if (itemMatch == NA_STRING || itemPattern == NA_STRING) {
      pResult[i] = NA_LOGICAL;
      continue;
    }

    itemResult = GEOSRelatePatternMatch_r(handle, CHAR(itemMatch), CHAR(itemPattern));
    if (itemResult == 2) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i);
    }

    pResult[i] = itemResult;
  }

    UNPROTECT(1);
  return result;
}
