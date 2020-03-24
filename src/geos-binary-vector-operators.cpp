
#include "geos-operator.h"
using namespace Rcpp;

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

// [[Rcpp::export]]
LogicalVector cpp_is_disjoint(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new DisjointOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class TouchesOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSTouches_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_touches(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new TouchesOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class IntersectsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSIntersects_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_intersects(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new IntersectsOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class CrossesOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCrosses_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_crosses(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new CrossesOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class WithinOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSWithin_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_within(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new WithinOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class ContainsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSContains_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_contains(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new ContainsOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class OverlapsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSOverlaps_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_overlaps(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new OverlapsOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class EqualsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSEquals_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_equals(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new EqualsOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class CoversOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCovers_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_covers(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new CoversOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}

class CoveredByOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCoveredBy_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_covered_by(SEXP dataLeft, SEXP dataRight) {
  BinaryPredicateOperator* op = new CoveredByOperator();

  op->initProvider(dataLeft, dataRight);
  LogicalVector result = op->operate();
  op->finishProvider();

  return result;
}
