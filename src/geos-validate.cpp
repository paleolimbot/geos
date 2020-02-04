
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector geos_wkt_is_parseable(CharacterVector wkt) {
  LogicalVector output(wkt.size());
  GEOSContextHandle_t context = geos_init();
  GEOSWKTReader *wkt_reader = GEOSWKTReader_create_r(context);

  for (int i=0; i < wkt.size(); i++) {
    try {
      GEOSWKTReader_read_r(context, wkt_reader, wkt[i]);
      output[i] = true;
    } catch(std::exception e) {
      // don't know how to get the error message here...
      // importing geos::util::GEOSException or the parse exception
      // don't seem to work, despite a nice error message in R
      output[i] = false;
    }
  }

  GEOSWKTReader_destroy_r(context, wkt_reader);
  geos_finish(context);
  return output;
}

// [[Rcpp::export]]
LogicalVector geos_wkb_is_parseable(List wkb) {
  LogicalVector output(wkb.size());
  GEOSContextHandle_t context = geos_init();
  GEOSWKBReader *wkb_reader = GEOSWKBReader_create_r(context);

  for (int i=0; i < wkb.size(); i++) {
    try {
      RawVector r = wkb[i];
      GEOSWKBReader_read_r(context, wkb_reader, &(r[0]), r.size());
      output[i] = true;
    } catch(std::exception e) {
      // don't know how to get the error message here...
      // importing geos::util::GEOSException or the parse exception
      // don't seem to work, despite a nice error message in R
      output[i] = false;
    }
  }

  GEOSWKBReader_destroy_r(context, wkb_reader);
  geos_finish(context);
  return output;
}
