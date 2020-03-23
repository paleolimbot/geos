
#include "geos-unary-vector-operators.h"
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector geomcpp_unary_predicate(SEXP data, int predicate) {
  GeometryProvider* provider = resolve_provider(data);

  UnaryPredicateOperator* op = new UnaryPredicateOperator(predicate);
  op->initProvider(provider);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

// ------ unary vector operators --------

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

// --- Unary predicates

UnaryPredicateOperator::UnaryPredicateOperator(int predicate) {
  this->predicate = predicate;
}

bool UnaryPredicateOperator::operateNext(GEOSGeometry* geometry) {
  char result = this->operateNextGEOS(geometry);
  if (result == 2) {
    stop("Exception on binary predicate");
  } else if (result == 1) {
    return true;
  } else if (result == 0) {
    return  false;
  } else {
    stop("Unknown output from binary predicate");
  }
}

char UnaryPredicateOperator::operateNextGEOS(GEOSGeometry* geometry) {
  switch(this->predicate) {
  case UnaryPredicates::IS_EMPTY:
    return GEOSisEmpty_r(this->context, geometry);

  case UnaryPredicates::IS_SIMPLE:
    return GEOSisSimple_r(this->context, geometry);

  case UnaryPredicates::HAS_Z:
    return GEOSHasZ_r(this->context, geometry);

  case UnaryPredicates::IS_CLOSED:
    return GEOSisClosed_r(this->context, geometry);
  }

  stop("No such unary predicate");
}
