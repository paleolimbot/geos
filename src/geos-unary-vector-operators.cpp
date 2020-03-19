
#include "geos-base.h"
#include "geos-provider.h"
#include "geos-unary-vector-operators.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector geomcpp_is_empty(SEXP data) {
  GeometryProvider* provider = resolve_provider(data);

  IsEmptyOperator* op = new IsEmptyOperator();
  op->initProvider(provider);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

// --- base operator

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

// --- is_empty operator

bool IsEmptyOperator::operateNext(GEOSGeometry* geometry) {
  return GEOSGetNumCoordinates_r(this->context, geometry) == 0;
}


