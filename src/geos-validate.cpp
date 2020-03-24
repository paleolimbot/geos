
#include "geos-base.h"
#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cpp_validate_provider(SEXP data) {
  GeometryProvider* provider = resolve_provider(data);
  LogicalVector output(provider->size());
  GEOSContextHandle_t context = geos_init();
  provider->init(context);

  for (int i=0; i < provider->size(); i++) {
    try {
      provider->getNext();
      output[i] = true;
    } catch(std::exception e) {
      // don't know how to get the error message here...
      // importing geos::util::GEOSException or the parse exception
      // don't seem to work, despite a nice error message in R
      output[i] = false;
    }
  }

  provider->finish();
  geos_finish(context);
  return output;
}
