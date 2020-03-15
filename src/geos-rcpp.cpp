
// this is intended to do all the 'hard' GEOS memory management so that other files
// can focus on geom operations

#include "geos-rcpp.h"
#include "geos-coords.h"
#include <Rcpp.h>

using namespace Rcpp;

static void __errorHandler(const char *fmt, ...) {

  char buf[BUFSIZ], *p;
  va_list ap;
  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);
  va_end(ap);
  p = buf + strlen(buf) - 1;
  if(strlen(buf) > 0 && *p == '\n') *p = '\0';

  Rcpp::Function error(".stop_geos", Rcpp::Environment::namespace_env("geom"));
  error(buf);

  return;
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

static void __countErrorHandler(const char *fmt, void *userdata) {
  int *i = (int *) userdata;
  *i = *i + 1;
}

static void __emptyNoticeHandler(const char *fmt, void *userdata) { }

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

// ---------- geometry provider implementations -------------


// --- base provider

void GeometryProvider::init(GEOSContextHandle_t context) {
  this->context = context;
}


void GeometryProvider::finish() {

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

// --- nested geotbl exporter

NestedGeoTblExporter::NestedGeoTblExporter() {

}

void NestedGeoTblExporter::init(GEOSContextHandle_t context, size_t size) {
  List data(size);
  data.attr("class") = "nested_geo_tbl";
  this->data = data;
  this->context = context;
  this->counter = 0;
}

void NestedGeoTblExporter::putNext(GEOSGeometry* geometry) {
  data[this->counter] = geometry_to_geo_tbl(this->context, geometry, this->counter + 1);
  this->counter = this->counter + 1;
}

SEXP NestedGeoTblExporter::finish() {
  return this->data;
}

// ---------- geometry provider resolvers -------------

GeometryProvider* resolve_provider(SEXP data) {
  if (Rf_inherits(data, "geo_wkt")) {
    return new WKTGeometryProvider((CharacterVector) data);
  } else if(Rf_inherits(data, "geo_wkb")) {
    return new WKBGeometryProvider((List) data);
  }

  stop("Can't resolve GeometryProvider");
}

GeometryExporter* resolve_exporter(SEXP ptype) {
  if (Rf_inherits(ptype, "geo_wkt")) {
    return new WKTGeometryExporter();
  } else if(Rf_inherits(ptype, "geo_wkb")) {
    return new WKBGeometryExporter();
  } else if(Rf_inherits(ptype, "geo_tbl")) {
    return new NestedGeoTblExporter();
  }

  stop("Can't resolve GeometryProvider");
}

// ------------- unary operators ----------------

UnaryGeometryOperator::UnaryGeometryOperator(GeometryProvider* provider,
                                             GeometryExporter* exporter) {
  this->provider = provider;
  this->exporter = exporter;
}

void UnaryGeometryOperator::init() {

}

void UnaryGeometryOperator::initBase() {
  this->context = geos_init();
  this->provider->init(this->context);
  this->exporter->init(this->context, this->provider->size());
}

SEXP UnaryGeometryOperator::operate() {
  this->initBase();
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometry;
  GEOSGeometry* result;

  try {
    for (int i=0; i < this->size(); i++) {
      checkUserInterrupt();
      geometry = this->provider->getNext();
      result = this->operateNext(geometry);
      this->exporter->putNext(result);
    }
  } catch(std::exception e) {
    this->finish();
    throw e;
  }

  this->finish();
  return this->finishBase();
}

void UnaryGeometryOperator::finish() {

}

SEXP UnaryGeometryOperator::finishBase() {
  this->provider->finish();
  SEXP value = this->exporter->finish();
  geos_finish(this->context);
  return value;
}

size_t UnaryGeometryOperator::size() {
  return this->provider->size();
}

// --- identity operator

IdentityOperator::IdentityOperator(GeometryProvider* provider, GeometryExporter* exporter) :
  UnaryGeometryOperator(provider, exporter) {
}

GEOSGeometry* IdentityOperator::operateNext(GEOSGeometry* geometry) {
  return geometry;
}

// --- buffer operator

BufferOperator::BufferOperator(GeometryProvider* provider, GeometryExporter* exporter,
                               double width, int quadSegs,
                               int endCapStyle, int joinStyle, double mitreLimit,
                               int singleSided) :
  UnaryGeometryOperator(provider, exporter) {
  this->width = width;
  this->endCapStyle = endCapStyle;
  this->joinStyle = joinStyle;
  this->mitreLimit = mitreLimit;
  this->quadSegs = quadSegs;
  this->singleSided = singleSided;
}

void BufferOperator::init() {
  this->params = GEOSBufferParams_create_r(this->context);
  GEOSBufferParams_setEndCapStyle_r(this->context, this->params, this->endCapStyle);
  GEOSBufferParams_setJoinStyle_r(this->context, this->params, this->joinStyle);
  GEOSBufferParams_setMitreLimit_r(this->context, this->params, this->mitreLimit);
  GEOSBufferParams_setQuadrantSegments_r(this->context, this->params, this->quadSegs);
  GEOSBufferParams_setSingleSided_r(this->context, this->params, this->singleSided);
}

GEOSGeometry* BufferOperator::operateNext(GEOSGeometry* geometry) {
  return GEOSBufferWithParams_r(this->context, geometry, this->params, this->width);
}

void BufferOperator::finish() {
  GEOSBufferParams_destroy_r(this->context, this->params);
}

// ------------- binary operators ----------------

BinaryGeometryOperator::BinaryGeometryOperator(GeometryProvider* providerLeft,
                                               GeometryProvider* providerRight,
                                               GeometryExporter* exporter) {
  this->providerLeft = providerLeft;
  this->providerRight = providerRight;
  this->exporter = exporter;
}

void BinaryGeometryOperator::init() {
  this->context = geos_init();
  this->providerLeft->init(this->context);
  this->providerRight->init(this->context);

  // check sizes: left and right must be equal
  if (this->providerLeft->size() != this->providerRight->size()) {
    stop("Providers with two different lengths passed to BinaryGeometryOperator");
  }

  this->exporter->init(this->context, this->providerLeft->size());
}

SEXP BinaryGeometryOperator::operate() {
  this->init();

  // TODO: there is probably a memory leak here, but
  // GEOSGeom_destroy_r(this->context, geometry) gives
  // an error
  GEOSGeometry* geometryLeft;
  GEOSGeometry* geometryRight;
  GEOSGeometry* result;

  try {
    for (int i=0; i < this->size(); i++) {
      checkUserInterrupt();
      geometryLeft = this->providerLeft->getNext();
      geometryRight = this->providerRight->getNext();
      result = this->operateNext(geometryLeft, geometryRight);
      this->exporter->putNext(result);
    }
  } catch(std::exception e) {
    this->finish();
    throw e;
  }

  return this->finish();
}

SEXP BinaryGeometryOperator::finish() {
  this->providerLeft->finish();
  this->providerRight->finish();
  SEXP value = this->exporter->finish();
  geos_finish(this->context);
  return value;
}

size_t BinaryGeometryOperator::size() {
  return this->providerLeft->size();
}



