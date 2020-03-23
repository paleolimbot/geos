
#include "geos-unary-operators.h"
using namespace Rcpp;

// [[Rcpp::export]]
SEXP geomcpp_buffer(SEXP data, SEXP ptype, NumericVector width, int quadSegs,
                    int endCapStyle, int joinStyle, double mitreLimit,
                    int singleSided) {
  GeometryProvider* provider = resolve_provider(data);
  GeometryExporter* exporter = resolve_exporter(ptype);

  BufferOperator* op = new BufferOperator(
    width, quadSegs,
    endCapStyle, joinStyle, mitreLimit,
    singleSided
  );

  op->initProvider(provider, exporter);
  SEXP result = op->operate();
  op->finishProvider();

  return result;
}

// [[Rcpp::export]]
SEXP geomcpp_convert(SEXP data, SEXP ptype) {
  GeometryProvider* provider = resolve_provider(data);
  GeometryExporter* exporter = resolve_exporter(ptype);

  IdentityOperator* op = new IdentityOperator();
  op->initProvider(provider, exporter);
  SEXP result = op->operate();
  op->finishProvider();

  return result;
}

// --- identity operator

GEOSGeometry* IdentityOperator::operateNext(GEOSGeometry* geometry) {
  return geometry;
}

// --- buffer operator

BufferOperator::BufferOperator(NumericVector width, int quadSegs,
                               int endCapStyle, int joinStyle, double mitreLimit,
                               int singleSided) {
  this->width = width;
  this->endCapStyle = endCapStyle;
  this->joinStyle = joinStyle;
  this->mitreLimit = mitreLimit;
  this->quadSegs = quadSegs;
  this->singleSided = singleSided;
}

size_t BufferOperator::maxParameterLength() {
  return (this->width).size();
}

void BufferOperator::init() {
  this->params = GEOSBufferParams_create_r(this->context);
  GEOSBufferParams_setEndCapStyle_r(this->context, this->params, this->endCapStyle);
  GEOSBufferParams_setJoinStyle_r(this->context, this->params, this->joinStyle);
  GEOSBufferParams_setMitreLimit_r(this->context, this->params, this->mitreLimit);
  GEOSBufferParams_setQuadrantSegments_r(this->context, this->params, this->quadSegs);
  GEOSBufferParams_setSingleSided_r(this->context, this->params, this->singleSided);
}

GEOSGeometry* BufferOperator::operateNext(GEOSGeometry* geometry) {
  return GEOSBufferWithParams_r(this->context, geometry, this->params, this->width[this->counter]);
}

void BufferOperator::finish() {
  GEOSBufferParams_destroy_r(this->context, this->params);
}
