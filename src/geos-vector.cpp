
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
  for (size_t i=0; i < size; i++) {
    checkUserInterrupt();

    if (NumericVector::is_na(ax0[i]) || NumericVector::is_na(ay0[i]) ||
        NumericVector::is_na(ax1[i]) || NumericVector::is_na(ay1[i]) ||
        NumericVector::is_na(bx0[i]) || NumericVector::is_na(by0[i]) ||
        NumericVector::is_na(bx1[i]) || NumericVector::is_na(by1[i])) {
      xOut[i] = NA_REAL;
      yOut[i] = NA_REAL;
    } else {
#ifdef HAVE361
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
#else
      stop("GEOS >= 3.6.1 required for GEOSSegmentIntersection_r");
#endif
    }
  }
  geos_finish(context);

  List out = List::create(_["x"] = xOut, _["y"] = yOut);
  return geo_coord_reclass(out, "geo_xy");
}

// [[Rcpp::export]]
IntegerVector cpp_orientation_index(NumericVector ax, NumericVector ay,
                                    NumericVector bx, NumericVector by,
                                    NumericVector px, NumericVector py) {
  size_t size = ax.size();
  IntegerVector index(size);

  GEOSContextHandle_t context = geos_init();
  for (size_t i=0; i < size; i++) {
    checkUserInterrupt();

    if (NumericVector::is_na(ax[i]) || NumericVector::is_na(ay[i]) ||
        NumericVector::is_na(bx[i]) || NumericVector::is_na(ay[i]) ||
        NumericVector::is_na(px[i]) || NumericVector::is_na(py[i])) {
      index[i] = NA_INTEGER;
    } else {
      index[i] = GEOSOrientationIndex_r(
        context,
        ax[i], ay[i],
        bx[i], by[i],
        px[i], py[i]
      );
    }
  }
  geos_finish(context);

  return index;
}
