
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

  virtual char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) = 0;
};

class DisjointOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSDisjoint_r(this->context, geometryLeft, geometryRight);
  }
};

class TouchesOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSTouches_r(this->context, geometryLeft, geometryRight);
  }
};

class IntersectsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSIntersects_r(this->context, geometryLeft, geometryRight);
  }
};

class CrossesOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCrosses_r(this->context, geometryLeft, geometryRight);
  }
};

class WithinOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSWithin_r(this->context, geometryLeft, geometryRight);
  }
};

class ContainsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSContains_r(this->context, geometryLeft, geometryRight);
  }
};

class OverlapsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSOverlaps_r(this->context, geometryLeft, geometryRight);
  }
};

class EqualsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSEquals_r(this->context, geometryLeft, geometryRight);
  }
};

class CoversOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCovers_r(this->context, geometryLeft, geometryRight);
  }
};

class CoveredByOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCoveredBy_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_binary_predicate(SEXP dataLeft, SEXP dataRight, int predicate) {
  BinaryPredicateOperator* op;

  switch(predicate) {
  case BinaryPredicates::DISJOINT:
    op = new DisjointOperator();
    break;

  case BinaryPredicates::TOUCHES:
    op = new TouchesOperator();

  case BinaryPredicates::INTERSECTS:
    op = new IntersectsOperator();
    break;

  case BinaryPredicates::CROSSES:
    op = new CrossesOperator();
    break;

  case BinaryPredicates::WITHIN:
    op = new WithinOperator();
    break;

  case BinaryPredicates::CONTAINS:
    op = new ContainsOperator();
    break;

  case BinaryPredicates::OVERLAPS:
    op = new OverlapsOperator();
    break;

  case BinaryPredicates::EQUALS:
    op = new EqualsOperator();
    break;

  case BinaryPredicates::COVERS:
    op = new CoversOperator();
    break;

  case BinaryPredicates::COVERED_BY:
    op = new CoveredByOperator();
    break;

  default:
    stop("No such binary predicate");
  }

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}
