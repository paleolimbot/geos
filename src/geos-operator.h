
#ifndef GEOS_OPERATOR_H
#define GEOS_OPERATOR_H

#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

// ------------ base class ------------

class Operator {
public:
  size_t commonSize;
  size_t counter;
  GEOSContextHandle_t context;

  virtual SEXP operate() = 0;
  virtual void init();
  virtual size_t maxParameterLength();
  virtual void finish();
  virtual size_t size();
  virtual void finishProvider();
  virtual ~Operator();
};

// ------------- unary operators ----------------

class UnaryGeometryOperator: public Operator {
public:
  std::unique_ptr<GeometryProvider> provider;
  std::unique_ptr<GeometryExporter> exporter;

  virtual void initProvider(SEXP provider, SEXP exporter);
  virtual SEXP operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometry) = 0;

private:
  void initBase();
  SEXP finishBase();
};

// ----- unary vector operators -----

template <class VectorType, class ScalarType>
class UnaryVectorOperator: public Operator {
public:
  std::unique_ptr<GeometryProvider> provider;
  VectorType data;

  virtual void initProvider(SEXP provider);
  virtual SEXP operate();
  virtual ScalarType operateNext(GEOSGeometry* geometry) = 0;

private:
  void initBase();
  VectorType finishBase();
};

// ------------- binary operators ----------------

class BinaryGeometryOperator: public Operator {
public:
  std::unique_ptr<GeometryProvider> providerLeft;
  std::unique_ptr<GeometryProvider> providerRight;
  std::unique_ptr<GeometryExporter> exporter;

  virtual void initProvider(SEXP providerLeft, SEXP providerRight, SEXP exporter);
  virtual SEXP operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) = 0;

private:
  void initBase();
  SEXP finishBase();
};

// ---------- binary vector operators -------------

template <class VectorType, class ScalarType>
class BinaryVectorOperator: public Operator {
public:
  std::unique_ptr<GeometryProvider> providerLeft;
  std::unique_ptr<GeometryProvider> providerRight;
  VectorType data;

  virtual void initProvider(SEXP providerLeft, SEXP providerRight);
  virtual SEXP operate();
  virtual ScalarType operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) = 0;

private:
  void initBase();
  VectorType finishBase();
};

// ------ unary vector operators implementation --------
// I don't know  why these can't be defined in geos-operator.cpp
// but putting them there results in a linker error

template <class VectorType, class ScalarType>
void UnaryVectorOperator<VectorType, ScalarType>::initProvider(SEXP provider) {
  this->provider = resolve_provider(provider);
}

template <class VectorType, class ScalarType>
void UnaryVectorOperator<VectorType, ScalarType>::initBase() {
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

  VectorType data(this->size());
  this->data = data;
}

template <class VectorType, class ScalarType>
SEXP UnaryVectorOperator<VectorType, ScalarType>::operate() {
  this->initBase();
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometry;
  ScalarType result;

  try {
    for (size_t i=0; i < this->size(); i++) {
      checkUserInterrupt();
      this->counter = i;
      geometry = this->provider->getNext();
      result = this->operateNext(geometry);
      this->data[i] = result;
    }
  } catch(std::exception e) {
    this->finish();
    throw e;
  }

  this->finish();
  return this->finishBase();
}

template <class VectorType, class ScalarType>
VectorType UnaryVectorOperator<VectorType, ScalarType>::finishBase() {
  this->provider->finish();
  geos_finish(this->context);
  return this->data;
}

// ------ binary vector operators implementation --------
// I don't know  why these can't be defined in geos-operator.cpp
// but putting them there results in a linker error

template <class VectorType, class ScalarType>
void BinaryVectorOperator<VectorType, ScalarType>::initProvider(SEXP providerLeft, SEXP providerRight) {
  this->providerLeft = resolve_provider(providerLeft);
  this->providerRight = resolve_provider(providerRight);
}

template <class VectorType, class ScalarType>
void BinaryVectorOperator<VectorType, ScalarType>::initBase() {
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
      stop("Providers with incompatible lengths passed to BinaryVectorOperator");
    }
  }

  VectorType data(this->size());
  this->data = data;
}

template <class VectorType, class ScalarType>
SEXP BinaryVectorOperator<VectorType, ScalarType>::operate() {
  this->initBase();
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometryLeft;
  GEOSGeometry* geometryRight;
  ScalarType result;

  try {
    for (size_t i=0; i < this->size(); i++) {
      checkUserInterrupt();
      this->counter = i;
      geometryLeft = this->providerLeft->getNext();
      geometryRight = this->providerRight->getNext();
      result = this->operateNext(geometryLeft, geometryRight);
      this->data[i] = result;
    }
  } catch(std::exception e) {
    this->finish();
    throw e;
  }

  this->finish();
  return this->finishBase();
}


template <class VectorType, class ScalarType>
VectorType BinaryVectorOperator<VectorType, ScalarType>::finishBase() {
  this->providerLeft->finish();
  this->providerRight->finish();
  geos_finish(this->context);
  return this->data;
}

# endif
