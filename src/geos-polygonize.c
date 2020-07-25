
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

SEXP geos_c_polygonize(SEXP collection) {
  if (collection == R_NilValue) {
    return R_NilValue;
  }

  GEOSGeometry* collectionGeometry = (GEOSGeometry*) R_ExternalPtrAddr(collection);
  if (collectionGeometry == NULL) {
    Rf_error("`collection` is not a valid external pointer");
  }

  GEOS_INIT();

  unsigned int nGeoms = GEOSGetNumGeometries_r(handle, collectionGeometry);
  // returns -1 on error, but not sure how to make this error
  if (nGeoms == -1) {
    GEOS_ERROR("%s", ""); // # nocov
  }

  const GEOSGeometry* geoms[nGeoms];
  for (unsigned int i = 0; i < nGeoms; i++) {
    geoms[i] = GEOSGetGeometryN_r(handle, collectionGeometry, i);
  }

  GEOSGeometry* result = GEOSPolygonize_r(handle, geoms, nGeoms);
  // not sure how to make this fire
  if (result == NULL) {
    GEOS_ERROR("%s", "Error calling polygonize: "); // # nocov
  }

  GEOS_FINISH();
  return geos_common_geometry_xptr(result);
}
