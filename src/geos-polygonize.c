
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

#define GEOS_POLYGONIZE(_func)                                 \
  if (collection == R_NilValue) {                                \
    return R_NilValue;                                           \
  }                                                              \
                                                                 \
  GEOSGeometry* collectionGeometry = (GEOSGeometry*) R_ExternalPtrAddr(collection);\
  if (collectionGeometry == NULL) {                              \
    Rf_error("`collection` is not a valid external pointer");    \
  }                                                              \
                                                                 \
  GEOS_INIT();                                                   \
                                                                 \
  unsigned int nGeoms = GEOSGetNumGeometries_r(handle, collectionGeometry);\
  if (nGeoms == -1) {                                            \
    GEOS_ERROR("%s", "");                                        \
  }                                                              \
                                                                 \
  const GEOSGeometry* geoms[nGeoms];                             \
  for (unsigned int i = 0; i < nGeoms; i++) {                    \
    geoms[i] = GEOSGetGeometryN_r(handle, collectionGeometry, i);\
  }                                                              \
                                                                 \
  GEOSGeometry* result = _func(handle, geoms, nGeoms);           \
  if (result == NULL) {                                          \
    GEOS_ERROR("%s", "Error calling polygonize: ");              \
  }                                                              \
                                                                 \
  GEOS_FINISH();                                                 \
  return geos_common_geometry_xptr(result);


SEXP geos_c_polygonize(SEXP collection) {
  GEOS_POLYGONIZE(GEOSPolygonize_r)
}

SEXP geos_c_polygonize_valid(SEXP collection) {
  GEOS_POLYGONIZE(GEOSPolygonize_valid_r)
}

SEXP geos_c_polygonize_cut_edges(SEXP collection) {
  GEOS_POLYGONIZE(GEOSPolygonizer_getCutEdges_r)
}

SEXP geos_c_polygonize_full(SEXP collection) {
  if (collection == R_NilValue) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 4));
    SET_VECTOR_ELT(result, 0, R_NilValue);
    SET_VECTOR_ELT(result, 1, R_NilValue);
    SET_VECTOR_ELT(result, 2, R_NilValue);
    SET_VECTOR_ELT(result, 3, R_NilValue);

    UNPROTECT(1); // result
    return result;
  }

  GEOSGeometry* collectionGeometry = (GEOSGeometry*) R_ExternalPtrAddr(collection);
  if (collectionGeometry == NULL) {
    Rf_error("`collection` is not a valid external pointer");
  }

  GEOS_INIT();

  GEOSGeometry* cuts = NULL;
  GEOSGeometry* dangles = NULL;
  GEOSGeometry* invalidRings = NULL;
  GEOSGeometry* resultGeometry = GEOSPolygonize_full_r(handle, collectionGeometry, &cuts, &dangles, &invalidRings);

  // rchk gives an error about these variables being unprotected,
  // possibly because of the Rf_allocVector() below
  SEXP resultPtr = PROTECT(geos_common_geometry_xptr(resultGeometry));
  SEXP cutsPtr = PROTECT(geos_common_geometry_xptr(cuts));
  SEXP danglesPtr = PROTECT(geos_common_geometry_xptr(dangles));
  SEXP invalidRingsPtr = PROTECT(geos_common_geometry_xptr(invalidRings));

  // don't know how to make the polygonizer fail
  if (resultGeometry == NULL) {
    UNPROTECT(4);
    GEOS_ERROR("%s: ", "Error calling polygonize full"); // # nocov
  }

  GEOS_FINISH();

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(result, 0, resultPtr);
  SET_VECTOR_ELT(result, 1, cutsPtr);
  SET_VECTOR_ELT(result, 2, danglesPtr);
  SET_VECTOR_ELT(result, 3, invalidRingsPtr);

  UNPROTECT(5); // result + children
  return result;
}
