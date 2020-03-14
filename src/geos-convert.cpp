
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// most of this is inspired by
// https://github.com/r-spatial/sf/blob/master/src/geos.cpp

std::vector<GeomPtr> geos_from_wkt(GEOSContextHandle_t context, CharacterVector wkt) {
  std::vector<GeomPtr> output(wkt.size());
  WKTGeometryProvider* provider = new WKTGeometryProvider(wkt);
  provider->init(context);

  for (int i=0; i < wkt.size(); i++) {
    GEOSGeometry* geometry;
    geometry = provider->getNext();
    output[i] = geos_ptr(geometry, context);
  }

  provider->finish();
  return output;
}

CharacterVector geos_to_wkt(GEOSContextHandle_t context, std::vector<GeomPtr> & vec_pointer) {
  CharacterVector output(vec_pointer.size());
  WKTGeometryExporter* exporter = new WKTGeometryExporter(output);
  exporter->init(context);

  for (int i=0; i < vec_pointer.size(); i++) {
    exporter->putNext(vec_pointer[i].get());
  }

  exporter->finish();
  return output;
}

std::vector<GeomPtr> geos_from_wkb(GEOSContextHandle_t context, List wkb) {
  std::vector<GeomPtr> output(wkb.size());
  WKBGeometryProvider* provider = new WKBGeometryProvider(wkb);
  provider->init(context);

  for (int i=0; i < wkb.size(); i++) {
    GEOSGeometry* geometry;
    geometry = provider->getNext();
    output[i] = geos_ptr(geometry, context);
  }

  provider->finish();
  return output;
}

List geos_to_wkb(GEOSContextHandle_t context, std::vector<GeomPtr> & vec_pointer) {
  List output(vec_pointer.size());
  WKBGeometryExporter* exporter = new WKBGeometryExporter(output);
  exporter->init(context);

  for (int i=0; i < vec_pointer.size(); i++) {
    exporter->putNext(vec_pointer[i].get());
  }

  exporter->finish();
  return output;
}


// [[Rcpp::export]]
CharacterVector geos_test_roundtrip_wkt(CharacterVector wkt) {
  // input
  WKTGeometryProvider* provider = new WKTGeometryProvider(wkt);

  // output
  CharacterVector output(provider->size());
  WKTGeometryExporter* exporter = new WKTGeometryExporter(output);

  // operator
  IdentityOperator* op = new IdentityOperator(provider, exporter);
  op->operate();

  return output;
}

// [[Rcpp::export]]
List geos_test_roundtrip_wkb(List wkb) {
  GEOSContextHandle_t context = geos_init();
  std::vector<GeomPtr> vec_pointer = geos_from_wkb(context, wkb);
  List output = geos_to_wkb(context, vec_pointer);
  geos_finish(context);
  return output;
}

// [[Rcpp::export]]
List geos_wkt_to_wkb(CharacterVector wkt) {
  GEOSContextHandle_t context = geos_init();
  std::vector<GeomPtr> vec_pointer = geos_from_wkt(context, wkt);
  List output = geos_to_wkb(context, vec_pointer);
  geos_finish(context);
  return output;
}

// [[Rcpp::export]]
CharacterVector geos_wkb_to_wkt(List wkb) {
  GEOSContextHandle_t context = geos_init();
  std::vector<GeomPtr> vec_pointer = geos_from_wkb(context, wkb);
  CharacterVector output = geos_to_wkt(context, vec_pointer);
  geos_finish(context);
  return output;
}
