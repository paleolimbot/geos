
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
