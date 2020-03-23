
#ifndef GEOS_UNARY_OPERATORS_H
#define GEOS_UNARY_OPERATORS_H

#include "geos-operator.h"
using namespace Rcpp;

SEXP geomcpp_buffer(SEXP data, SEXP ptype, NumericVector width, int quadSegs,
                    int endCapStyle, int joinStyle, double mitreLimit,
                    int singleSided);

SEXP geomcpp_convert(SEXP data, SEXP ptype);

// --- identity operator

class IdentityOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry);
};

// --- buffer operator

class BufferOperator: public UnaryGeometryOperator {
public:
  NumericVector width;
  int quadSegs;
  int endCapStyle;
  int joinStyle;
  double mitreLimit;
  int singleSided;
  GEOSBufferParams* params;

  BufferOperator(NumericVector width, int quadSegs,
                 int endCapStyle, int joinStyle, double mitreLimit,
                 int singleSided);
  size_t maxParameterLength();
  void init();
  GEOSGeometry* operateNext(GEOSGeometry* geometry);
  void finish();
};

#endif
