
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

  UnaryPredicateOperator(int predicate) {
    this->predicate = predicate;
  }

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

  char operateNextGEOS(GEOSGeometry* geometry) {
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
};

// [[Rcpp::export]]
LogicalVector geomcpp_unary_predicate(SEXP data, int predicate) {
  GeometryProvider* provider = resolve_provider(data);

  UnaryPredicateOperator* op = new UnaryPredicateOperator(predicate);
  op->initProvider(provider);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}
