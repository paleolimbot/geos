
// this is intended to do all the 'hard' GEOS memory management so that other files
// can focus on geom operations

#include "geos-rcpp.h"
#include <Rcpp.h>

using namespace Rcpp;

static void __errorHandler(const char *fmt, ...) { // #nocov start

  char buf[BUFSIZ], *p;
  va_list ap;
  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);
  va_end(ap);
  p = buf + strlen(buf) - 1;
  if(strlen(buf) > 0 && *p == '\n') *p = '\0';

  Rcpp::Function error(".stop_geos", Rcpp::Environment::namespace_env("geom"));
  error(buf);

  return; // #nocov end
}

static void __warningHandler(const char *fmt, ...) {

  char buf[BUFSIZ], *p;
  va_list ap;
  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);
  va_end(ap);
  p = buf + strlen(buf) - 1;
  if(strlen(buf) > 0 && *p == '\n') *p = '\0';

  Rcpp::Function warning("warning");
  warning(buf);

  return;
}

// #nocov start
static void __countErrorHandler(const char *fmt, void *userdata) {
  int *i = (int *) userdata;
  *i = *i + 1;
}

static void __emptyNoticeHandler(const char *fmt, void *userdata) { }
// #nocov end

GEOSContextHandle_t geos_init(void) {
#ifdef HAVE350
  GEOSContextHandle_t context = GEOS_init_r();
  GEOSContext_setNoticeHandler_r(context, __warningHandler);
  GEOSContext_setErrorHandler_r(context, __errorHandler);
  return context;
#else
  return initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
#endif
}

void geos_finish(GEOSContextHandle_t context) {
#ifdef HAVE350
  GEOS_finish_r(context);
#else
  finishGEOS_r(context);
#endif
}

GeomPtr geos_ptr(GEOSGeometry* g, GEOSContextHandle_t context) {
  auto deleter = std::bind(GEOSGeom_destroy_r, context, std::placeholders::_1);
  return GeomPtr(g, deleter);
}


// ---------- geometry provider implementations -------------


// --- base provider

void GeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
}


void GeometryProvider::finish() {

}

// --- base exporter

template <class OutputType>
void GeometryExporter<OutputType>::init(GEOSContextHandle_t context) {
  this->context = context;
}

template <class OutputType>
OutputType GeometryExporter<OutputType>::getData() {
  return this->data;
}

template <class OutputType>
void GeometryExporter<OutputType>::finish() {

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

WKTGeometryExporter::WKTGeometryExporter(CharacterVector data) {
  this->data = data;
  this->counter = 0;
}

void WKTGeometryExporter::init(GEOSContextHandle_t context) {
  this->context = context;
  this->wkt_writer = GEOSWKTWriter_create_r(context);
}

void WKTGeometryExporter::putNext(GEOSGeometry* geometry) {
  std::string wkt_single;
  wkt_single = GEOSWKTWriter_write_r(this->context, wkt_writer, geometry);
  this->data[this->counter] = wkt_single;
  this->counter = this->counter + 1;
}

void WKTGeometryExporter::finish() {
  GEOSWKTWriter_destroy_r(this->context, this->wkt_writer);
}

size_t WKTGeometryExporter::size() {
  return (this->data).size();
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

WKBGeometryExporter::WKBGeometryExporter(List data) {
  this->data = data;
  this->counter = 0;
}

void WKBGeometryExporter::init(GEOSContextHandle_t context) {
  this->context = context;
  this->wkb_writer = GEOSWKBWriter_create_r(context);
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

void WKBGeometryExporter::finish() {
  GEOSWKBWriter_destroy_r(this->context, this->wkb_writer);
}

size_t WKBGeometryExporter::size() {
  return (this->data).size();
}

