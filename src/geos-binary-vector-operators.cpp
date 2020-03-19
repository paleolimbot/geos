
#include "geos-binary-vector-operators.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector geomcpp_binary_predicate(SEXP dataLeft, SEXP dataRight, int predicate) {
  GeometryProvider* providerLeft = resolve_provider(dataLeft);
  GeometryProvider* providerRight = resolve_provider(dataRight);

  BinaryPredicateOperator* op = new  BinaryPredicateOperator(predicate);
  op->initProvider(providerLeft, providerRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

// --- base operator

template <class VectorType, class ScalarType>
void BinaryVectorOperator<VectorType, ScalarType>::initProvider(GeometryProvider* providerLeft,
                                                                GeometryProvider* providerRight) {
  this->providerLeft = providerLeft;
  this->providerRight = providerRight;
}

template <class VectorType, class ScalarType>
size_t BinaryVectorOperator<VectorType, ScalarType>::maxParameterLength() {
  return 1;
}

template <class VectorType, class ScalarType>
void BinaryVectorOperator<VectorType, ScalarType>::init() {

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

  for (int i=0; i<nonConstantSizes.size(); i++) {
    if (nonConstantSizes[i] != this->commonSize) {
      stop("Providers with incompatible lengths passed to BinaryVectorOperator");
    }
  }

  VectorType data(this->size());
  this->data = data;
}

template <class VectorType, class ScalarType>
VectorType BinaryVectorOperator<VectorType, ScalarType>::operate() {
  this->initBase();
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometryLeft;
  GEOSGeometry* geometryRight;
  ScalarType result;

  try {
    for (int i=0; i < this->size(); i++) {
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
void BinaryVectorOperator<VectorType, ScalarType>::finish() {

}

template <class VectorType, class ScalarType>
VectorType BinaryVectorOperator<VectorType, ScalarType>::finishBase() {
  this->providerLeft->finish();
  this->providerRight->finish();
  geos_finish(this->context);
  return this->data;
}

template <class VectorType, class ScalarType>
void BinaryVectorOperator<VectorType, ScalarType>::finishProvider() {

}

template <class VectorType, class ScalarType>
size_t BinaryVectorOperator<VectorType, ScalarType>::size() {
  return this->commonSize;
}

// --- binary predicate wrapper

BinaryPredicateOperator::BinaryPredicateOperator(int predicate) {
  this->predicate = predicate;
}

bool BinaryPredicateOperator::operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
  char result = this->operateNextGEOS(geometryLeft, geometryRight);
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

char BinaryPredicateOperator::operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
  switch(this->predicate) {
  case BinaryPredicates::DISJOINT:
    return GEOSDisjoint_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::TOUCHES:
    return GEOSTouches_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::INTERSECTS:
    return GEOSIntersects_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::CROSSES:
    return GEOSCrosses_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::WITHIN:
    return GEOSWithin_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::CONTAINS:
    return GEOSContains_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::OVERLAPS:
    return GEOSOverlaps_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::EQUALS:
    return GEOSEquals_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::COVERS:
    return GEOSCovers_r(this->context, geometryLeft, geometryRight);

  case BinaryPredicates::COVERED_BY:
    return GEOSCoveredBy_r(this->context, geometryLeft, geometryRight);
  }

  stop("No such binary predicate");
}
