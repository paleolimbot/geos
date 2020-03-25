
#include "geos-operator.h"
using namespace Rcpp;

class OffsetCurveOperator: public UnaryGeometryOperator {
public:
  NumericVector width;
  int quadSegs;
  int endCapStyle;
  int joinStyle;
  double mitreLimit;

  OffsetCurveOperator(NumericVector width, int quadSegs,
                      int endCapStyle, int joinStyle, double mitreLimit) {
    this->width = width;
    this->endCapStyle = endCapStyle;
    this->joinStyle = joinStyle;
    this->mitreLimit = mitreLimit;
    this->quadSegs = quadSegs;
  }

  size_t maxParameterLength() {
    return (this->width).size();
  }

  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSOffsetCurve_r(
      this->context,
      geometry,
      this->width[this->counter],
      this->quadSegs,
      this->joinStyle,
      this->mitreLimit
    );
  }
};

// [[Rcpp::export]]
SEXP cpp_offset_curve(SEXP x, NumericVector width, int quadSegs,
                      int endCapStyle, int joinStyle, double mitreLimit,
                      SEXP to) {
  OffsetCurveOperator op(
      width, quadSegs,
      endCapStyle, joinStyle, mitreLimit
  );
  op.initProvider(x, to);
  return op.operate();
}

class BufferOperator: public OffsetCurveOperator {
public:
  int singleSided;
  GEOSBufferParams* params;

  BufferOperator(NumericVector width, int quadSegs,
                 int endCapStyle, int joinStyle, double mitreLimit,
                 int singleSided) :
    OffsetCurveOperator(width, quadSegs, endCapStyle, joinStyle, mitreLimit) {
    this->singleSided = singleSided;
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
SEXP cpp_buffer(SEXP x, NumericVector width, int quadSegs,
                int endCapStyle, int joinStyle, double mitreLimit,
                int singleSided, SEXP to) {
  BufferOperator op(
    width, quadSegs,
    endCapStyle, joinStyle, mitreLimit,
    singleSided
  );
  op.initProvider(x, to);
  return op.operate();
}
