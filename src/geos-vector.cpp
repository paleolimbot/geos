
#include "geos-base.h"
#include "geos-coords.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_segment_intersection(NumericVector ax0, NumericVector ay0,
                              NumericVector ax1, NumericVector ay1,
                              NumericVector bx0, NumericVector by0,
                              NumericVector bx1, NumericVector by1) {
  size_t size = ax0.size();
  NumericVector xOut(size);
  NumericVector yOut(size);

  double cx;
  double cy;

  GEOSContextHandle_t context = geos_init();
  for (size_t i=0; i < ax0.size(); i++) {
    checkUserInterrupt();

    GEOSSegmentIntersection_r(
      context,
      ax0[i], ay0[i],
      ax1[i], ay1[i],
      bx0[i], by0[i],
      bx1[i], by1[i],
      &cx, &cy
    );

    xOut[i] = cx;
    yOut[i] = cy;
  }
  geos_finish(context);

  List out = List::create(_["x"] = xOut, _["y"] = yOut);
  return geo_coord_reclass(out, "geo_xy");
}
