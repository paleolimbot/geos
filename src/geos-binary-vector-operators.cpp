
#include "geos-operator.h"
using namespace Rcpp;

enum BinaryPredicates {
  DISJOINT = 1,
  TOUCHES = 2,
  INTERSECTS = 3,
  CROSSES = 4,
  WITHIN = 5,
  CONTAINS = 6,
  OVERLAPS = 7,
  EQUALS = 8,
  EQUALS_EXACT = 9,
  COVERS = 10,
  COVERED_BY = 11
};

class BinaryPredicateOperator: public BinaryVectorOperator<LogicalVector, bool> {
public:
  int predicate;

  BinaryPredicateOperator(int predicate) {
    this->predicate = predicate;
  }

  bool operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
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

  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
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
};

// [[Rcpp::export]]
LogicalVector geomcpp_binary_predicate(SEXP dataLeft, SEXP dataRight, int predicate) {
  BinaryPredicateOperator* op = new  BinaryPredicateOperator(predicate);

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}
