
#include "geos-binary-operators.h"
using namespace Rcpp;

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

// --- intersection!

GEOSGeometry* IntersectionOperator::operateNext(GEOSGeometry* geometryLeft, GEOSGeometry* geometryRight) {
  return GEOSIntersection_r(this->context, geometryLeft, geometryRight);
}
