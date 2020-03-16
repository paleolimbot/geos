
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP geomcpp_convert(SEXP data, SEXP ptype) {
  GeometryProvider* provider = resolve_provider(data);
  GeometryExporter* exporter = resolve_exporter(ptype);

  IdentityOperator* op = new IdentityOperator(provider, exporter);
  return op->operate();
}
