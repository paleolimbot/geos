
#ifndef GEOS_UNARY_VECTOR_OPERATORS_H
#define GEOS_UNARY_VECTOR_OPERATORS_H

#include "geos-base.h"
#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

template <class VectorType, class ScalarType>
class UnaryVectorOperator {
public:
  GeometryProvider* provider;
  VectorType data;
  GEOSContextHandle_t context;
  size_t commonSize;
  size_t counter;

  virtual size_t maxParameterLength();
  virtual void initProvider(GeometryProvider* provider);
  virtual void init();
  virtual VectorType operate();
  virtual ScalarType operateNext(GEOSGeometry* geometry) = 0;
  virtual void finish();
  virtual void finishProvider();

  virtual size_t size();

private:
  void initBase();
  VectorType finishBase();
};


class IsEmptyOperator: public UnaryVectorOperator<LogicalVector, bool> {
  bool operateNext(GEOSGeometry* geometry);
};

#endif
