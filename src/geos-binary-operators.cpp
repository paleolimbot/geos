
#include "geos-base.h"
#include "geos-binary-operators.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP geomcpp_intersection(SEXP dataLeft, SEXP dataRight, SEXP ptype) {
  GeometryProvider* providerLeft = resolve_provider(dataLeft);
  GeometryProvider* providerRight = resolve_provider(dataRight);
  GeometryExporter* exporter = resolve_exporter(ptype);

  IntersectionOperator* op = new IntersectionOperator();
  op->initProvider(providerLeft, providerRight, exporter);
  SEXP result = op->operate();
  op->finishProvider();

  return result;
}

// ------------- binary operators ----------------

void BinaryGeometryOperator::initProvider(GeometryProvider* providerLeft,
                                          GeometryProvider* providerRight,
                                          GeometryExporter* exporter) {
  this->providerLeft = providerLeft;
  this->providerRight = providerRight;
  this->exporter = exporter;
}

size_t BinaryGeometryOperator::maxParameterLength() {
  return 1;
}

void BinaryGeometryOperator::initBase() {
  this->context = geos_init();
  this->providerLeft->init(this->context);
  this->providerRight->init(this->context);

  IntegerVector allSizes = IntegerVector::create(
    this->maxParameterLength(),
    this->providerLeft->size(),
    this->providerRight->size()
  );

  IntegerVector nonConstantSizes = allSizes[allSizes != 1];
  if (nonConstantSizes.size() == 0) {
    this->commonSize = 1;
  } else {
    this->commonSize = nonConstantSizes[0];
  }

  for (int i=0; i<nonConstantSizes.size(); i++) {
    if (nonConstantSizes[i] != this->commonSize) {
      stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
    }
  }

  this->exporter->init(this->context, this->commonSize);
}

void BinaryGeometryOperator::init() {

}

SEXP BinaryGeometryOperator::operate() {
  this->initBase();
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

  this->finish();
  return this->finishBase();
}

void BinaryGeometryOperator::finish() {

}

SEXP BinaryGeometryOperator::finishBase() {
  this->providerLeft->finish();
  this->providerRight->finish();
  SEXP value = this->exporter->finish();
  geos_finish(this->context);
  return value;
}

void BinaryGeometryOperator::finishProvider() {

}

size_t BinaryGeometryOperator::size() {
  return this->commonSize;
}

// --- intersection!

GEOSGeometry* IntersectionOperator::operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
  return GEOSIntersection_r(this->context, geometryLeft, geometryRight);
}
