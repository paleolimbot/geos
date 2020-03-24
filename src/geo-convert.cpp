
#include "geos-operator.h"
using namespace Rcpp;

class IdentityOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return geometry;
  }
};

// [[Rcpp::export]]
SEXP cpp_convert(SEXP data, SEXP ptype) {
  IdentityOperator op;
  op.initProvider(data, ptype);
  return op.operate();
}
