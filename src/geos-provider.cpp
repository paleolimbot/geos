
#include "geos-provider.h"
#include "geos-coords.h"
#include <Rcpp.h>
using namespace Rcpp;

// ---------- geometry provider implementations -------------

// --- base provider

void GeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
}


void GeometryProvider::finish() {

}

// --- constant provider

ConstantGeometryProvider::ConstantGeometryProvider(GeometryProvider* baseProvider) {
  this->baseProvider = baseProvider;
}

void ConstantGeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->baseProvider->init(context);
  this->geometry = nullptr;
}

GEOSGeometry* ConstantGeometryProvider::getNext() {
  if (this->geometry == nullptr) {
    this->geometry = this->baseProvider->getNext();
  }
  return this->geometry;
}

void ConstantGeometryProvider::finish() {
  this->baseProvider->finish();
}

size_t ConstantGeometryProvider::size() {
  return 1;
}

// --- base exporter

void GeometryExporter::init(GEOSContextHandle_t context, size_t size) {
  this->context = context;
}

SEXP GeometryExporter::finish() {
  return R_NilValue;
}

// --- WKT provider

WKTGeometryProvider::WKTGeometryProvider(CharacterVector data) {
  this->data = data;
  this->counter = 0;
}

void WKTGeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->wkt_reader = GEOSWKTReader_create_r(context);
}

GEOSGeometry* WKTGeometryProvider::getNext() {
  GEOSGeometry* geometry = GEOSWKTReader_read_r(
    this->context,
    this->wkt_reader,
    this->data[this->counter]
  );
  this->counter = this->counter + 1;
  return geometry;
}

void WKTGeometryProvider::finish() {
  GEOSWKTReader_destroy_r(this->context, this->wkt_reader);
}

size_t WKTGeometryProvider::size() {
  return (this->data).size();
}

// --- WKT exporter

WKTGeometryExporter::WKTGeometryExporter() {
  this->counter = 0;
}

void WKTGeometryExporter::init(GEOSContextHandle_t context, size_t size) {
  this->context = context;
  this->wkt_writer = GEOSWKTWriter_create_r(context);
  CharacterVector data(size);
  this->data = data;
}

void WKTGeometryExporter::putNext(GEOSGeometry* geometry) {
  std::string wkt_single;
  wkt_single = GEOSWKTWriter_write_r(this->context, wkt_writer, geometry);
  this->data[this->counter] = wkt_single;
  this->counter = this->counter + 1;
}

SEXP WKTGeometryExporter::finish() {
  GEOSWKTWriter_destroy_r(this->context, this->wkt_writer);
  return this->data;
}

// --- WKB provider

WKBGeometryProvider::WKBGeometryProvider(List data) {
  this->data = data;
  this->counter = 0;
}

void WKBGeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
  this->wkb_reader = GEOSWKBReader_create_r(context);
}

GEOSGeometry* WKBGeometryProvider::getNext() {
  RawVector r = this->data[this->counter];
  GEOSGeometry* geometry = GEOSWKBReader_read_r(context, this->wkb_reader, &(r[0]), r.size());
  this->counter = this->counter + 1;
  return geometry;
}

void WKBGeometryProvider::finish() {
  GEOSWKBReader_destroy_r(this->context, this->wkb_reader);
}

size_t WKBGeometryProvider::size() {
  return (this->data).size();
}

// --- WKB exporter

WKBGeometryExporter::WKBGeometryExporter() {
  this->counter = 0;
}

void WKBGeometryExporter::init(GEOSContextHandle_t context, size_t size) {
  this->context = context;
  this->wkb_writer = GEOSWKBWriter_create_r(context);
  List data(size);
  this->data = data;
}

void WKBGeometryExporter::putNext(GEOSGeometry* geometry) {
  size_t size;
  unsigned char *buf = GEOSWKBWriter_write_r(this->context, this->wkb_writer, geometry, &size);
  RawVector raw(size);
  memcpy(&(raw[0]), buf, size);
  GEOSFree_r(this->context, buf);

  this->data[this->counter] = raw;
  this->counter = this->counter + 1;
}

SEXP WKBGeometryExporter::finish() {
  GEOSWKBWriter_destroy_r(this->context, this->wkb_writer);
  return data;
}

// --- nested GeoCoord exporter

NestedGeoCoordExporter::NestedGeoCoordExporter() {

}

void NestedGeoCoordExporter::init(GEOSContextHandle_t context, size_t size) {
  List data(size);
  data.attr("class") = "nested_geo_coord";
  this->data = data;
  this->context = context;
  this->counter = 0;
}

void NestedGeoCoordExporter::putNext(GEOSGeometry* geometry) {
  data[this->counter] = geometry_to_geo_coord(this->context, geometry, this->counter + 1);
  this->counter = this->counter + 1;
}

SEXP NestedGeoCoordExporter::finish() {
  return this->data;
}

// ---------- geometry provider resolvers -------------

GeometryProvider* resolve_provider(SEXP data) {
  if (Rf_inherits(data, "geo_wkt")) {
    CharacterVector dataChar = (CharacterVector) data;
    GeometryProvider* wktProvider = new WKTGeometryProvider(dataChar);
    if (dataChar.size() ==  1) {
      return new ConstantGeometryProvider(wktProvider);
    } else {
      return wktProvider;
    }
  } else if(Rf_inherits(data, "geo_wkb")) {
    List dataList = (List) data;
    GeometryProvider* wkbProvider = new WKBGeometryProvider(dataList);
    if (dataList.size() ==  1) {
      return new ConstantGeometryProvider(wkbProvider);
    } else {
      return wkbProvider;
    }
    return new WKBGeometryProvider((List) data);
  }

  stop("Can't resolve GeometryProvider");
}

GeometryExporter* resolve_exporter(SEXP ptype) {
  if (Rf_inherits(ptype, "geo_wkt")) {
    return new WKTGeometryExporter();
  } else if(Rf_inherits(ptype, "geo_wkb")) {
    return new WKBGeometryExporter();
  } else if(Rf_inherits(ptype, "geo_coord")) {
    return new NestedGeoCoordExporter();
  }

  stop("Can't resolve GeometryProvider");
}
