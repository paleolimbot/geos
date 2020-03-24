
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

  bool operateNext(GEOSGeometry* geometry)  {
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

  virtual char operateNextGEOS(GEOSGeometry* geometry) = 0;
};

class IsEmptyOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisEmpty_r(this->context, geometry);
  }
};

class IsSimpleOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisSimple_r(this->context, geometry);
  }
};

class HasZOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSHasZ_r(this->context, geometry);
  }
};

class IsClosedOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisClosed_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_unary_predicate(SEXP data, int predicate) {
  UnaryPredicateOperator* op;

  switch(predicate) {
  case UnaryPredicates::IS_EMPTY:
    op = new IsEmptyOperator();
    break;

  case UnaryPredicates::IS_SIMPLE:
    op = new IsSimpleOperator();
    break;

  case UnaryPredicates::HAS_Z:
    op = new HasZOperator();
    break;

  case UnaryPredicates::IS_CLOSED:
    op = new IsClosedOperator();
    break;

  default:
    stop("No such unary predicate");
  }

  op->initProvider(data);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}
