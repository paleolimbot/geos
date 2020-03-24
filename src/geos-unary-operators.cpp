
#include "geos-operator.h"
using namespace Rcpp;

class BufferOperator: public UnaryGeometryOperator {
public:
  NumericVector width;
  int quadSegs;
  int endCapStyle;
  int joinStyle;
  double mitreLimit;
  int singleSided;
  GEOSBufferParams* params;

  BufferOperator(NumericVector width, int quadSegs,
                 int endCapStyle, int joinStyle, double mitreLimit,
                 int singleSided) {
    this->width = width;
    this->endCapStyle = endCapStyle;
    this->joinStyle = joinStyle;
    this->mitreLimit = mitreLimit;
    this->quadSegs = quadSegs;
    this->singleSided = singleSided;
  }

  size_t maxParameterLength() {
    return (this->width).size();
  }

  void init() {
    this->params = GEOSBufferParams_create_r(this->context);
    GEOSBufferParams_setEndCapStyle_r(this->context, this->params, this->endCapStyle);
    GEOSBufferParams_setJoinStyle_r(this->context, this->params, this->joinStyle);
    GEOSBufferParams_setMitreLimit_r(this->context, this->params, this->mitreLimit);
    GEOSBufferParams_setQuadrantSegments_r(this->context, this->params, this->quadSegs);
    GEOSBufferParams_setSingleSided_r(this->context, this->params, this->singleSided);
  }

  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSBufferWithParams_r(this->context, geometry, this->params, this->width[this->counter]);
  }

  void finish()  {
    GEOSBufferParams_destroy_r(this->context, this->params);
  }
};

// [[Rcpp::export]]
SEXP cpp_buffer(SEXP data, SEXP ptype, NumericVector width, int quadSegs,
                    int endCapStyle, int joinStyle, double mitreLimit,
                    int singleSided) {
  BufferOperator* op = new BufferOperator(
    width, quadSegs,
    endCapStyle, joinStyle, mitreLimit,
    singleSided
  );
  return cpp_do_operate(op, data, ptype);
}


class IdentityOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return geometry;
  }
};

// [[Rcpp::export]]
SEXP cpp_convert(SEXP data, SEXP ptype) {
  IdentityOperator* op = new IdentityOperator();
  return cpp_do_operate(op, data, ptype);
}
