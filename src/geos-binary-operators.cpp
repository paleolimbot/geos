
#include "geos-base.h"
#include "geos-binary-operators.h"
#include <Rcpp.h>
using namespace Rcpp;


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

  // check sizes: left and right must be equal
  if (this->providerLeft->size() != this->providerRight->size()) {
    stop("Providers with two different lengths passed to BinaryGeometryOperator");
  }

  this->exporter->init(this->context, this->providerLeft->size());
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
  return this->providerLeft->size();
}


