
#include "geos-operator.h"
using namespace Rcpp;

class IntersectionOperator: public BinaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSIntersection_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
SEXP cpp_intersection(SEXP dataLeft, SEXP dataRight, SEXP ptype) {
  IntersectionOperator op;
  op.initProvider(dataLeft, dataRight, ptype);
  return op.operate();
}

class DifferenceOperator: public BinaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSDifference_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
SEXP cpp_difference(SEXP dataLeft, SEXP dataRight, SEXP ptype) {
  DifferenceOperator op;
  op.initProvider(dataLeft, dataRight, ptype);
  return op.operate();
}

class SymDifferenceOperator: public BinaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSSymDifference_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
SEXP cpp_sym_difference(SEXP dataLeft, SEXP dataRight, SEXP ptype) {
  SymDifferenceOperator op;
  op.initProvider(dataLeft, dataRight, ptype);
  return op.operate();
}

class UnionOperator: public BinaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSUnion_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
SEXP cpp_union(SEXP dataLeft, SEXP dataRight, SEXP ptype) {
  UnionOperator op;
  op.initProvider(dataLeft, dataRight, ptype);
  return op.operate();
}

class UnaryUnionOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSUnaryUnion_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_unary_union(SEXP dataLeft, SEXP ptype) {
  UnaryUnionOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class CoverageUnionOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
#ifdef HAVE380
    return GEOSCoverageUnion_r(this->context, geometry);
#else
    stop("Need GEOS >= 3.8.0 to use GEOSCoverageUnion_r()");
#endif
  }
};

// [[Rcpp::export]]
SEXP cpp_coverage_union(SEXP dataLeft, SEXP ptype) {
  CoverageUnionOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class ClipByRectOperator: public UnaryGeometryOperator {
public:
  NumericVector xmin;
  NumericVector ymin;
  NumericVector xmax;
  NumericVector ymax;

  ClipByRectOperator(NumericVector xmin, NumericVector ymin, NumericVector xmax, NumericVector ymax) {
    this->xmin = xmin;
    this->ymin = ymin;
    this->xmax = xmax;
    this->ymax = ymax;
  }

  size_t maxParameterLength() {
    return this->xmin.size();
  }

  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSClipByRect_r(
      this->context,
      geometry,
      this->xmin[this->counter],
      this->ymin[this->counter],
      this->xmax[this->counter],
      this->ymax[this->counter]
    );
  }
};

// [[Rcpp::export]]
SEXP cpp_clip_by_rect(SEXP dataLeft,
                      NumericVector xmin, NumericVector ymin,
                      NumericVector xmax, NumericVector ymax,
                      SEXP to) {
  ClipByRectOperator op(xmin, ymin, xmax, ymax);
  op.initProvider(dataLeft, to);
  return op.operate();
}
