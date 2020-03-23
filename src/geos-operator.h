
#ifndef GEOS_OPERATOR_H
#define GEOS_OPERATOR_H

#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

// ------------- unary operators ----------------

class UnaryGeometryOperator {
public:
  GeometryProvider* provider;
  GeometryExporter* exporter;
  GEOSContextHandle_t context;
  size_t commonSize;
  size_t counter;

  virtual size_t maxParameterLength();
  virtual void initProvider(GeometryProvider* provider, GeometryExporter* exporter);
  virtual void init();
  virtual SEXP operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometry) = 0;
  virtual void finish();
  virtual void finishProvider();

  virtual size_t size();

private:
  void initBase();
  SEXP finishBase();
};

// ----- unary vector operators -----

template <class VectorType, class ScalarType>
class UnaryVectorOperator {
public:
  GeometryProvider* provider;
  VectorType data;
  GEOSContextHandle_t context;
  size_t commonSize;
  size_t counter;

  virtual size_t maxParameterLength();
  virtual void initProvider(GeometryProvider* provider);
  virtual void init();
  virtual VectorType operate();
  virtual ScalarType operateNext(GEOSGeometry* geometry) = 0;
  virtual void finish();
  virtual void finishProvider();

  virtual size_t size();

private:
  void initBase();
  VectorType finishBase();
};

// ------------- binary operators ----------------

class BinaryGeometryOperator {
public:
  GeometryProvider* providerLeft;
  GeometryProvider* providerRight;
  GeometryExporter* exporter;
  GEOSContextHandle_t context;
  int commonSize;

  virtual void initProvider(GeometryProvider* providerLeft,
                            GeometryProvider* providerRight,
                            GeometryExporter* exporter);
  virtual size_t maxParameterLength();
  virtual void init();
  virtual SEXP operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) = 0;
  virtual void finish();
  virtual void finishProvider();

  virtual size_t size();

private:
  void initBase();
  SEXP finishBase();
};

// ------ unary vector operators implementation --------
// I don't know  why these can't be defined in geos-operator.cpp
// but putting them there results in a linker error

template <class VectorType, class ScalarType>
void UnaryVectorOperator<VectorType, ScalarType>::initProvider(GeometryProvider* provider) {
  this->provider = provider;
}

template <class VectorType, class ScalarType>
size_t UnaryVectorOperator<VectorType, ScalarType>::maxParameterLength() {
  return 1;
}

template <class VectorType, class ScalarType>
void UnaryVectorOperator<VectorType, ScalarType>::init() {

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

  for (int i=0; i<nonConstantSizes.size(); i++) {
    if (nonConstantSizes[i] != this->commonSize) {
      stop("Providers with incompatible lengths passed to BinaryGeometryOperator");
    }
  }

  VectorType data(this->size());
  this->data = data;
}

template <class VectorType, class ScalarType>
VectorType UnaryVectorOperator<VectorType, ScalarType>::operate() {
  this->initBase();
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometry;
  ScalarType result;

  try {
    for (int i=0; i < this->size(); i++) {
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
void UnaryVectorOperator<VectorType, ScalarType>::finish() {

}

template <class VectorType, class ScalarType>
VectorType UnaryVectorOperator<VectorType, ScalarType>::finishBase() {
  this->provider->finish();
  geos_finish(this->context);
  return this->data;
}

template <class VectorType, class ScalarType>
void UnaryVectorOperator<VectorType, ScalarType>::finishProvider() {

}

template <class VectorType, class ScalarType>
size_t UnaryVectorOperator<VectorType, ScalarType>::size() {
  return this->commonSize;
}

# endif
