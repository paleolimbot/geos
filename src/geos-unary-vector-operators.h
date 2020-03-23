
#ifndef GEOS_UNARY_VECTOR_OPERATORS_H
#define GEOS_UNARY_VECTOR_OPERATORS_H

#include "geos-operator.h"
using namespace Rcpp;

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
