
#include "geos-operator.h"
using namespace Rcpp;

class IntersectionOperator: public BinaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
    return GEOSIntersection_r(this->context, geometryLeft, geometryRight);
  }
};

// [[Rcpp::export]]
SEXP geomcpp_intersection(SEXP dataLeft, SEXP dataRight, SEXP ptype) {
  IntersectionOperator* op = new IntersectionOperator();

  op->initProvider(dataLeft, dataRight, ptype);
  SEXP result = op->operate();
  op->finishProvider();

  return result;
}
