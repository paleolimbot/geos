
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector geomcpp_convert_wkt(SEXP data) {
  GeometryProvider* provider = resolve_provider(data);

  CharacterVector output(provider->size());
  WKTGeometryExporter* exporter = new WKTGeometryExporter(output);

  IdentityOperator* op = new IdentityOperator(provider, exporter);
  op->operate();

  return output;
}

// [[Rcpp::export]]
List geomcpp_convert_wkb(SEXP data) {
  GeometryProvider* provider = resolve_provider(data);

  List output(provider->size());
  WKBGeometryExporter* exporter = new WKBGeometryExporter(output);

  IdentityOperator* op = new IdentityOperator(provider, exporter);
  op->operate();

  return output;
}
