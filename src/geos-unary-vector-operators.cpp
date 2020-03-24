
#include "geos-operator.h"
using namespace Rcpp;

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

// [[Rcpp::export]]
LogicalVector cpp_is_empty(SEXP data) {
  UnaryPredicateOperator* op = new IsEmptyOperator();

  op->initProvider(data);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class IsSimpleOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisSimple_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_simple(SEXP data) {
  UnaryPredicateOperator* op = new IsSimpleOperator();

  op->initProvider(data);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class HasZOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSHasZ_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_has_z(SEXP data) {
  UnaryPredicateOperator* op = new HasZOperator();

  op->initProvider(data);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class IsClosedOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisClosed_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_closed(SEXP data) {
  UnaryPredicateOperator* op = new IsClosedOperator();

  op->initProvider(data);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}
