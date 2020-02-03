
#include "geos-rcpp.h"
#include <Rcpp.h>

// [[Rcpp::export]]
std::string geos_version_impl() {
  return GEOSversion();
}
