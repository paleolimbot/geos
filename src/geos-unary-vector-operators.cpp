
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
  IsEmptyOperator op;
  op.initProvider(data);
  return op.operate();
}

class IsSimpleOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisSimple_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_simple(SEXP data) {
  IsSimpleOperator op;
  op.initProvider(data);
  return op.operate();
}

class HasZOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSHasZ_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_has_z(SEXP data) {
  HasZOperator op;
  op.initProvider(data);
  return op.operate();
}

class IsClosedOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisClosed_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_closed(SEXP data) {
  IsClosedOperator op;
  op.initProvider(data);
  return op.operate();
}
