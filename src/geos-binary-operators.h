
#ifndef GEOS_BINARY_OPERATORS_H
#define GEOS_BINARY_OPERATORS_H

#include "geos-base.h"
#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

SEXP geomcpp_intersection(SEXP dataLeft, SEXP dataRight, SEXP ptype);

// ------------- binary operators ----------------

class BinaryGeometryOperator {
public:
  GeometryProvider* providerLeft;
  GeometryProvider* providerRight;
  GeometryExporter* exporter;
  GEOSContextHandle_t context;
  int commonSize;

  virtual void initProvider(GeometryProvider* providerLeft,
                            GeometryProvider* providerRight,
                            GeometryExporter* exporter);
  virtual void init();
  virtual SEXP operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) = 0;
  virtual void finish();
  virtual void finishProvider();

  virtual size_t size();

private:
  void initBase();
  SEXP finishBase();
};

class IntersectionOperator: public BinaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight);
};

#endif
