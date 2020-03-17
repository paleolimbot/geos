
#ifndef GEOS_BINARY_OPERATORS_H
#define GEOS_BINARY_OPERATORS_H

#include "geos-base.h"
#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

// ------------- binary operators ----------------

class BinaryGeometryOperator {
public:
  GeometryProvider* providerLeft;
  GeometryProvider* providerRight;
  GeometryExporter* exporter;
  GEOSContextHandle_t context;

  BinaryGeometryOperator(GeometryProvider* providerLeft,
                         GeometryProvider* providerRight,
                         GeometryExporter* exporter);

  virtual void init();
  virtual SEXP operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) = 0;
  virtual SEXP finish();

  virtual size_t size();
};

#endif
