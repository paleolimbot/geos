
#ifndef GEOS_RCPP_H
#define GEOS_RCPP_H

// prevents using non-thread-safe GEOSxx functions without _r extension.
#define GEOS_USE_ONLY_R_API
#include <geos_c.h>
#include <memory>
#include <vector>
#include <Rcpp.h>

// version constants
#if GEOS_VERSION_MAJOR == 3
# if GEOS_VERSION_MINOR >= 5
#  define HAVE350
# endif
# if GEOS_VERSION_MINOR >= 8
#  define HAVE380
# endif
# if GEOS_VERSION_MINOR == 6
#  if GEOS_VERSION_PATCH >= 1
#   define HAVE361
#  endif
# endif
# if GEOS_VERSION_MINOR >= 7
#  define HAVE361
#  define HAVE370
# endif
#else
# if GEOS_VERSION_MAJOR > 3
#  define HAVE350
#  define HAVE370
#  define HAVE361
#  define HAVE380
# endif
#endif

// function declarations
GEOSContextHandle_t geos_init(void);
void geos_finish(GEOSContextHandle_t context);

// vectors of GeomPtr s are used for most operations
typedef std::unique_ptr<GEOSGeometry, std::function<void(GEOSGeometry*)>> GeomPtr;
GeomPtr geos_ptr(GEOSGeometry* g, GEOSContextHandle_t context);

// ---------- geometry provider/exporter definitions -------------

// --- base

class GeometryProvider {
public:
  GEOSContextHandle_t context;

  virtual void init(GEOSContextHandle_t context);
  virtual GEOSGeometry* getNext() = 0;
  virtual void finish();
  virtual size_t size() = 0;
};

class GeometryExporter {
public:
  GEOSContextHandle_t context;

  virtual void init(GEOSContextHandle_t context);
  virtual void putNext(GEOSGeometry* geometry) = 0;
  virtual void finish();
  virtual size_t size() = 0;
};

using namespace Rcpp;

// --- WKT

class WKTGeometryProvider: public GeometryProvider {
public:
  CharacterVector data;
  GEOSWKTReader *wkt_reader;
  size_t counter;

  WKTGeometryProvider(CharacterVector data);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  void finish();
  size_t size();
};

class WKTGeometryExporter: public GeometryExporter {
public:
  CharacterVector data;
  GEOSWKTWriter *wkt_writer;
  size_t counter;

  WKTGeometryExporter(CharacterVector data);
  void init(GEOSContextHandle_t context);
  void putNext(GEOSGeometry* geometry);
  void finish();
  size_t size();
};

// --- WKB

class WKBGeometryProvider: public GeometryProvider {
public:
  List data;
  GEOSWKBReader *wkb_reader;
  size_t counter;

  WKBGeometryProvider(List data);
  void init(GEOSContextHandle_t context);
  GEOSGeometry* getNext();
  void finish();
  size_t size();
};

class WKBGeometryExporter: public GeometryExporter {
public:
  List data;
  GEOSWKBWriter *wkb_writer;
  size_t counter;

  WKBGeometryExporter(List data);
  void init(GEOSContextHandle_t context);
  void putNext(GEOSGeometry* geometry);
  void finish();
  size_t size();
};


// ------------- unary operators ----------------

class UnaryGeometryOperator {
public:
  GeometryProvider* provider;
  GeometryExporter* exporter;
  GEOSContextHandle_t context;

  UnaryGeometryOperator(GeometryProvider* provider, GeometryExporter* exporter);

  virtual void init();
  virtual void operate();
  virtual GEOSGeometry* operateNext(GEOSGeometry* geometry) = 0;
  virtual void finish();

  virtual size_t size();
};

class IdentityOperator: public UnaryGeometryOperator {
public:
  IdentityOperator(GeometryProvider* provider, GeometryExporter* exporter);
  GEOSGeometry* operateNext(GEOSGeometry* geometry);
};

#endif
