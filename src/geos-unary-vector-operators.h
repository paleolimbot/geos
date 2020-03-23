
#ifndef GEOS_UNARY_VECTOR_OPERATORS_H
#define GEOS_UNARY_VECTOR_OPERATORS_H

#include "geos-operator.h"
using namespace Rcpp;

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

enum UnaryPredicates {
  IS_EMPTY = 1,
  IS_SIMPLE = 2,
  IS_RING = 3,
  HAS_Z = 4,
  IS_CLOSED = 5
};

class UnaryPredicateOperator: public UnaryVectorOperator<LogicalVector, bool> {
public:
  int predicate;

  UnaryPredicateOperator(int predicate);
  bool operateNext(GEOSGeometry* geometry);
  char operateNextGEOS(GEOSGeometry* geometry);
};

#endif
