
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
  DisjointOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class TouchesOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSTouches_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_touches(SEXP dataLeft, SEXP dataRight) {
  TouchesOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class IntersectsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSIntersects_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_intersects(SEXP dataLeft, SEXP dataRight) {
  IntersectsOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class CrossesOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCrosses_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_crosses(SEXP dataLeft, SEXP dataRight) {
  CrossesOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class WithinOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSWithin_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_within(SEXP dataLeft, SEXP dataRight) {
  WithinOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class ContainsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSContains_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_contains(SEXP dataLeft, SEXP dataRight) {
  ContainsOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class OverlapsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSOverlaps_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_overlaps(SEXP dataLeft, SEXP dataRight) {
  OverlapsOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class EqualsOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSEquals_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_equals(SEXP dataLeft, SEXP dataRight) {
  EqualsOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class CoversOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCovers_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_covers(SEXP dataLeft, SEXP dataRight) {
  CoversOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}

class CoveredByOperator: public BinaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSCoveredBy_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_covered_by(SEXP dataLeft, SEXP dataRight) {
  CoveredByOperator op;
  op.initProvider(dataLeft, dataRight);
  return op.operate();
}
