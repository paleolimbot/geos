
#include "geos-operator.h"
using namespace Rcpp;

class AreaOperator: public UnaryVectorOperator<NumericVector, double>{
  double operateNext(GEOSGeometry* geometry) {
    double area;
    GEOSArea_r(this->context, geometry, &area);
    return area;
  }
};

// [[Rcpp::export]]
NumericVector cpp_area(SEXP x) {
  AreaOperator op;
  op.initProvider(x);
  return op.operate();
}

class LengthOperator: public UnaryVectorOperator<NumericVector, double>{
  double operateNext(GEOSGeometry* geometry) {
    double length;
    GEOSLength_r(this->context, geometry, &length);
    return length;
  }
};

// [[Rcpp::export]]
NumericVector cpp_length(SEXP x) {
  LengthOperator op;
  op.initProvider(x);
  return op.operate();
}

class DistanceOperator: public BinaryVectorOperator<NumericVector, double>{
  double operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    double distance;
    GEOSDistance_r(this->context, geometryLeft, geometryRight, &distance);
    return distance;
  }
};

// [[Rcpp::export]]
NumericVector cpp_distance(SEXP x, SEXP y) {
  DistanceOperator op;
  op.initProvider(x, y);
  return op.operate();
}
