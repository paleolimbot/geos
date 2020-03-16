
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP geomcpp_buffer(SEXP data, SEXP ptype, double width, int quadSegs,
                    int endCapStyle, int joinStyle, double mitreLimit,
                    int singleSided) {
  GeometryProvider* provider = resolve_provider(data);
  GeometryExporter* exporter = resolve_exporter(ptype);

  BufferOperator* op = new BufferOperator(
    provider, exporter, width, quadSegs,
    endCapStyle, joinStyle, mitreLimit,
    singleSided
  );

  return op->operate();
}
