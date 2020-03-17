
#include "geos-base.h"
#include "geos-binary-operators.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP geomcpp_intersection(SEXP dataLeft, SEXP dataRight, SEXP ptype) {
  GeometryProvider* providerLeft = resolve_provider(dataLeft);
  GeometryProvider* providerRight = resolve_provider(dataRight);
  GeometryExporter* exporter = resolve_exporter(ptype);

  IntersectionOperator* op = new IntersectionOperator(providerLeft, providerRight, exporter);
  return op->operate();
}

// ------------- binary operators ----------------

BinaryGeometryOperator::BinaryGeometryOperator(GeometryProvider* providerLeft,
                                               GeometryProvider* providerRight,
                                               GeometryExporter* exporter) {
  this->providerLeft = providerLeft;
  this->providerRight = providerRight;
  this->exporter = exporter;
}

void BinaryGeometryOperator::init() {
  this->context = geos_init();
  this->providerLeft->init(this->context);
  this->providerRight->init(this->context);

  // check sizes
  if (this->providerLeft->size() == 0 || this->providerRight->size() == 0) {
    this->commonSize = 0;
  } else if(this->providerLeft->size() == 1) {
    this->commonSize = this->providerRight->size();
  } else if(this->providerRight->size() == 1) {
    this->commonSize = this->providerLeft->size();
  } else if(this->providerLeft->size() == this->providerRight->size()) {
    this->commonSize = this->providerLeft->size();
  } else {
    stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
  }

  this->exporter->init(this->context, this->commonSize);
}

SEXP BinaryGeometryOperator::operate() {
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometryLeft;
  GEOSGeometry* geometryRight;
  GEOSGeometry* result;

  try {
    for (int i=0; i < this->size(); i++) {
      checkUserInterrupt();
      geometryLeft = this->providerLeft->getNext();
      geometryRight = this->providerRight->getNext();
      result = this->operateNext(geometryLeft, geometryRight);
      this->exporter->putNext(result);
    }
  } catch(std::exception e) {
    this->finish();
    throw e;
  }

  return this->finish();
}

SEXP BinaryGeometryOperator::finish() {
  this->providerLeft->finish();
  this->providerRight->finish();
  SEXP value = this->exporter->finish();
  geos_finish(this->context);
  return value;
}

size_t BinaryGeometryOperator::size() {
  return this->commonSize;
}

// --- intersection!

IntersectionOperator::IntersectionOperator(GeometryProvider* providerLeft,
                                           GeometryProvider* providerRight,
                                           GeometryExporter* exporter) :
  BinaryGeometryOperator(providerLeft, providerRight, exporter) {
}

GEOSGeometry* IntersectionOperator::operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
  return GEOSIntersection_r(this->context, geometryLeft, geometryRight);
}
