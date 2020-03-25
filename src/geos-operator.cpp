
#include "geos-operator.h"
using namespace Rcpp;

// --------- base class ---------

void Operator::init() {

}

size_t Operator::maxParameterLength() {
  return 1;
}

void Operator::finish() {

}

size_t Operator::size() {
  return this->commonSize;
}

void Operator::finishProvider() {

}

Operator::~Operator() {

}

// ------------- unary operators ----------------

void UnaryGeometryOperator::initProvider(SEXP provider, SEXP exporter) {
  this->provider = resolve_provider(provider);
  this->exporter = resolve_exporter(exporter);
}

void UnaryGeometryOperator::initBase() {
  this->context = geos_init();
  this->provider->init(this->context);

  IntegerVector allSizes = IntegerVector::create(
    this->maxParameterLength(),
    this->provider->size()
  );

  IntegerVector nonConstantSizes = allSizes[allSizes != 1];
  if (nonConstantSizes.size() == 0) {
    this->commonSize = 1;
  } else {
    this->commonSize = nonConstantSizes[0];
  }

  for (size_t i=0; i<nonConstantSizes.size(); i++) {
    if (nonConstantSizes[i] != this->commonSize) {
      stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
    }
  }

  this->exporter->init(this->context, this->size());
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
    for (size_t i=0; i < this->size(); i++) {
      checkUserInterrupt();
      this->counter = i;
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

SEXP UnaryGeometryOperator::finishBase() {
  this->provider->finish();
  SEXP value = this->exporter->finish();
  geos_finish(this->context);
  return value;
}

// ------------- binary operators ----------------

void BinaryGeometryOperator::initProvider(SEXP providerLeft,
                                          SEXP providerRight,
                                          SEXP exporter) {
  this->providerLeft = resolve_provider(providerLeft);
  this->providerRight = resolve_provider(providerRight);
  this->exporter = resolve_exporter(exporter);
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

  for (size_t i=0; i<nonConstantSizes.size(); i++) {
    if (nonConstantSizes[i] != this->commonSize) {
      stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
    }
  }

  this->exporter->init(this->context, this->commonSize);
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
    for (size_t i=0; i < this->size(); i++) {
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

SEXP BinaryGeometryOperator::finishBase() {
  this->providerLeft->finish();
  this->providerRight->finish();
  SEXP value = this->exporter->finish();
  geos_finish(this->context);
  return value;
}
