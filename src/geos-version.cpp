
#include "geos-rcpp.h"
#include <Rcpp.h>

// [[Rcpp::export]]
std::string geos_version_impl() {
  return GEOSversion();
}

// [[Rcpp::export]]
void geos_test_throw_error() {
  GEOSContextHandle_t context = geos_init();
  // do something that will cause an error
  GEOSGeomFromWKT_r(context, "POINT NO COORDS");
  geos_finish(context);
}
