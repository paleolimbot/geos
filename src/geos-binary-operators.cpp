
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
  return cpp_do_operate(new IntersectionOperator(), dataLeft, dataRight, ptype);
}
