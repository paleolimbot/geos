
#ifndef GEOS_BINARY_OPERATORS_H
#define GEOS_BINARY_OPERATORS_H

#include "geos-base.h"
#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

template <class VectorType, class ScalarType>
class BinaryVectorOperator {
public:
  GeometryProvider* providerLeft;
  GeometryProvider* providerRight;
  VectorType data;
  GEOSContextHandle_t context;
  size_t commonSize;
  size_t counter;

  virtual size_t maxParameterLength();
  virtual void initProvider(GeometryProvider* providerLeft, GeometryProvider* providerRight);
  virtual void init();
  virtual VectorType operate();
  virtual ScalarType operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) = 0;
  virtual void finish();
  virtual void finishProvider();

  virtual size_t size();

private:
  void initBase();
  VectorType finishBase();
};

// --- binary predicate operators

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

  BinaryPredicateOperator(int predicate);
  bool operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight);
  char operateNextGEOS(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight);
};

#endif
