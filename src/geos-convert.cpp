
# include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// // [[Rcpp::export]]
// Rcpp::List wkt_to_geos_impl(Rcpp::CharacterVector wkt) {
//   Rcpp::List output (wkt.length());
//
//   // using advice from here:
//   // https://www.r-bloggers.com/external-pointers-with-rcpp/
//   // and source code from here:
//   // https://trac.osgeo.org/geos/browser/git/tests/geostest/geostest.c
//   GEOSContextHandle_t context;
//   context = geos_init();
//   for (int i=0; i<wkt.length(); i++) {
//     GEOSGeometry* g1;
//     g1 = GEOSGeomFromWKT_r(context, wkt[i]);
//     Rcpp::XPtr<GEOSGeometry> geomPtr(g1, true);
//     output[i] = geomPtr;
//   }
//
//   geos_finish(context);
//
//   return output;
// }
