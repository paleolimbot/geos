
// prevents using non-thread-safe GEOSxx functions without _r extension.
#define GEOS_USE_ONLY_R_API
#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string geos_version_impl() {
    return GEOSversion();
}
