
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector geomcpp_convert_wkt(SEXP data) {
  GeometryProvider* provider = resolve_provider(data);
  WKTGeometryExporter* exporter = new WKTGeometryExporter();

  IdentityOperator* op = new IdentityOperator(provider, exporter);
  return op->operate();
}

// [[Rcpp::export]]
List geomcpp_convert_wkb(SEXP data) {
  GeometryProvider* provider = resolve_provider(data);
  WKBGeometryExporter* exporter = new WKBGeometryExporter();

  IdentityOperator* op = new IdentityOperator(provider, exporter);
  return op->operate();
}
