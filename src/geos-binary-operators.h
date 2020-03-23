
#ifndef GEOS_BINARY_OPERATORS_H
#define GEOS_BINARY_OPERATORS_H

#include "geos-operator.h"
using namespace Rcpp;

SEXP geomcpp_intersection(SEXP dataLeft, SEXP dataRight, SEXP ptype);

class IntersectionOperator: public BinaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight);
};

#endif
