
#ifndef GEOS_UNARY_OPERATORS_H
#define GEOS_UNARY_OPERATORS_H

#include "geos-base.h"
#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

SEXP geomcpp_buffer(SEXP data, SEXP ptype, double width, int quadSegs,
                    int endCapStyle, int joinStyle, double mitreLimit,
                    int singleSided);

SEXP geomcpp_convert(SEXP data, SEXP ptype);

// ------------- unary operators ----------------

class UnaryGeometryOperator {
public:
  GeometryProvider* provider;
  GeometryExporter* exporter;
  GEOSContextHandle_t context;

  virtual void initProvider(GeometryProvider* provider, GeometryExporter* exporter);
  virtual void init();
  virtual SEXP operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometry) = 0;
  virtual void finish();
  virtual void finishProvider();

  virtual size_t size();

private:
  void initBase();
  SEXP finishBase();
};

// --- identity operator

class IdentityOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry);
};

// --- buffer operator

class BufferOperator: public UnaryGeometryOperator {
public:
  double width;
  int quadSegs;
  int endCapStyle;
  int joinStyle;
  double mitreLimit;
  int singleSided;
  GEOSBufferParams* params;

  BufferOperator(double width, int quadSegs,
                 int endCapStyle, int joinStyle, double mitreLimit,
                 int singleSided);
  void init();
  GEOSGeometry* operateNext(GEOSGeometry* geometry);
  void finish();
};

#endif
