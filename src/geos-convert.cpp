
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// most of this is inspired by
// https://github.com/r-spatial/sf/blob/master/src/geos.cpp

// need to find a way to get this in geos-rcpp.h
using GeomPtr = std::unique_ptr<GEOSGeometry, std::function<void(GEOSGeometry*)>>;

static GeomPtr geos_ptr(GEOSGeometry* g, GEOSContextHandle_t hGEOSctxt) {
  auto deleter = std::bind(GEOSGeom_destroy_r, hGEOSctxt, std::placeholders::_1);
  return GeomPtr(g, deleter);
}

std::vector<GeomPtr> geos_from_wkt(GEOSContextHandle_t context, CharacterVector wkt) {
  std::vector<GeomPtr> output(wkt.size());

  for (int i=0; i < wkt.size(); i++) {
    GEOSGeometry* geometry;
    // should be using GEOSWKTReader
    geometry = GEOSGeomFromWKT_r(context, wkt[i]);
    output[i] = geos_ptr(geometry, context);
  }

  return output;
}

CharacterVector geos_to_wkt(GEOSContextHandle_t context, std::vector<GeomPtr> & vec_pointer) {
  CharacterVector output(vec_pointer.size());

  for (int i=0; i < vec_pointer.size(); i++) {
    std::string wkt_single;
    // should be using GEOSWKTReader
    wkt_single = GEOSGeomToWKT_r(context, vec_pointer[i].get());
    output[i] = wkt_single;
  }

  return output;
}

// [[Rcpp::export]]
CharacterVector geos_test_roundtrip_wkt(CharacterVector wkt) {
  GEOSContextHandle_t context = geos_init();
  std::vector<GeomPtr> vec_pointer = geos_from_wkt(context, wkt);
  CharacterVector output = geos_to_wkt(context, vec_pointer);
  geos_finish(context);
  return output;
}
