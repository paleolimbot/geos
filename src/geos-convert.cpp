
#include "geos-rcpp.h"
#include <Rcpp.h>
using namespace Rcpp;

// most of this is inspired by
// https://github.com/r-spatial/sf/blob/master/src/geos.cpp

std::vector<GeomPtr> geos_from_wkt(GEOSContextHandle_t context, CharacterVector wkt) {
  std::vector<GeomPtr> output(wkt.size());
  GEOSWKTReader *wkt_reader = GEOSWKTReader_create_r(context);

  for (int i=0; i < wkt.size(); i++) {
    GEOSGeometry* geometry;
    geometry = GEOSWKTReader_read_r(context, wkt_reader, wkt[i]);
    output[i] = geos_ptr(geometry, context);
  }

  GEOSWKTReader_destroy_r(context, wkt_reader);
  return output;
}

CharacterVector geos_to_wkt(GEOSContextHandle_t context, std::vector<GeomPtr> & vec_pointer) {
  CharacterVector output(vec_pointer.size());
  GEOSWKTWriter *wkt_writer = GEOSWKTWriter_create_r(context);

  for (int i=0; i < vec_pointer.size(); i++) {
    std::string wkt_single;
    wkt_single = GEOSWKTWriter_write_r(context, wkt_writer, vec_pointer[i].get());
    output[i] = wkt_single;
  }

  GEOSWKTWriter_destroy_r(context, wkt_writer);
  return output;
}

std::vector<GeomPtr> geos_from_wkb(GEOSContextHandle_t context, List wkb) {
  std::vector<GeomPtr> output(wkb.size());
  GEOSWKBReader *wkb_reader = GEOSWKBReader_create_r(context);

  for (int i=0; i < wkb.size(); i++) {
    GEOSGeometry* geometry;
    RawVector r = wkb[i];
    geometry = GEOSWKBReader_read_r(context, wkb_reader, &(r[0]), r.size());
    output[i] = geos_ptr(geometry, context);
  }

  GEOSWKBReader_destroy_r(context, wkb_reader);
  return output;
}

List geos_to_wkb(GEOSContextHandle_t context, std::vector<GeomPtr> & vec_pointer) {
  List output(vec_pointer.size());
  GEOSWKBWriter *wkb_writer = GEOSWKBWriter_create_r(context);

  for (int i=0; i < vec_pointer.size(); i++) {
    std::string wkt_single;
    size_t size;
    unsigned char *buf = GEOSWKBWriter_write_r(context, wkb_writer, vec_pointer[i].get(), &size);
    RawVector raw(size);
    memcpy(&(raw[0]), buf, size);
    GEOSFree_r(context, buf);
    output[i] = raw;
  }

  GEOSWKBWriter_destroy_r(context, wkb_writer);
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
