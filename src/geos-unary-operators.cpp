
#include "geos-base.h"
#include "geos-provider.h"
#include "geos-unary-operators.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP geomcpp_buffer(SEXP data, SEXP ptype, double width, int quadSegs,
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

// ------------- unary operators ----------------

void UnaryGeometryOperator::initProvider(GeometryProvider* provider, GeometryExporter* exporter) {
  this->provider = provider;
  this->exporter = exporter;
}

void UnaryGeometryOperator::init() {

}

void UnaryGeometryOperator::initBase() {
  this->context = geos_init();
  this->provider->init(this->context);
  this->exporter->init(this->context, this->provider->size());
}

SEXP UnaryGeometryOperator::operate() {
  this->initBase();
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometry;
  GEOSGeometry* result;

  try {
    for (int i=0; i < this->size(); i++) {
      checkUserInterrupt();
      geometry = this->provider->getNext();
      result = this->operateNext(geometry);
      this->exporter->putNext(result);
    }
  } catch(std::exception e) {
    this->finish();
    throw e;
  }

  this->finish();
  return this->finishBase();
}

void UnaryGeometryOperator::finish() {

}

SEXP UnaryGeometryOperator::finishBase() {
  this->provider->finish();
  SEXP value = this->exporter->finish();
  geos_finish(this->context);
  return value;
}

void UnaryGeometryOperator::finishProvider() {

}

size_t UnaryGeometryOperator::size() {
  return this->provider->size();
}

// --- identity operator

GEOSGeometry* IdentityOperator::operateNext(GEOSGeometry* geometry) {
  return geometry;
}

// --- buffer operator

BufferOperator::BufferOperator(double width, int quadSegs,
                               int endCapStyle, int joinStyle, double mitreLimit,
                               int singleSided) {
  this->width = width;
  this->endCapStyle = endCapStyle;
  this->joinStyle = joinStyle;
  this->mitreLimit = mitreLimit;
  this->quadSegs = quadSegs;
  this->singleSided = singleSided;
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
  return GEOSBufferWithParams_r(this->context, geometry, this->params, this->width);
}

void BufferOperator::finish() {
  GEOSBufferParams_destroy_r(this->context, this->params);
}
