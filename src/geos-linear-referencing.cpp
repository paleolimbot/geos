
#include "geos-operator.h"
using namespace Rcpp;

class ProjectOperator: public BinaryVectorOperator<NumericVector, double> {
  double operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSProject_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
NumericVector cpp_project(SEXP dataLeft, SEXP dataRight) {
  ProjectOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class ProjectNormalizedOperator: public BinaryVectorOperator<NumericVector, double> {
  double operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSProjectNormalized_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
NumericVector cpp_project_normalized(SEXP dataLeft, SEXP dataRight) {
  ProjectNormalizedOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class InterpolateOperator: public UnaryGeometryOperator {
public:
  NumericVector distance;
  InterpolateOperator(NumericVector distance) {
    this->distance = distance;
  }

  size_t maxParameterLength() {
    return  this->distance.size();
  }

  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSInterpolate_r(this->context, geometry, this->distance[this->counter]);
  }
};

// [[Rcpp::export]]
SEXP cpp_interpolate(SEXP data, SEXP ptype, NumericVector distance) {
  InterpolateOperator op(distance);
  op.initProvider(data, ptype);
  return op.operate();
}

class InterpolateNormalizedOperator: public UnaryGeometryOperator {
public:
  NumericVector distanceNormalized;
  InterpolateNormalizedOperator(NumericVector distanceNormalized) {
    this->distanceNormalized = distanceNormalized;
  }

  size_t maxParameterLength() {
    return  this->distanceNormalized.size();
  }

  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSInterpolateNormalized_r(this->context, geometry, this->distanceNormalized[this->counter]);
  }
};

// [[Rcpp::export]]
SEXP cpp_interpolate_normalized(SEXP data, SEXP ptype, NumericVector distance) {
  InterpolateNormalizedOperator op(distance);
  op.initProvider(data, ptype);
  return op.operate();
}
