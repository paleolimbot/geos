
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
  GeometryProvider* providerLeft = resolve_provider(dataLeft);
  GeometryProvider* providerRight = resolve_provider(dataRight);
  GeometryExporter* exporter = resolve_exporter(ptype);

  IntersectionOperator* op = new IntersectionOperator();
  op->initProvider(providerLeft, providerRight, exporter);
  SEXP result = op->operate();
  op->finishProvider();

  return result;
}
