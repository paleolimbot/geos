
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

class GeometryProvider {
public:
  GEOSContextHandle_t context;

  void init(GEOSContextHandle_t context) {
    this->context = context;
  }

  virtual GEOSGeometry* getNext() = 0;

  void finish() {

  }

  virtual size_t size() = 0;
};

using namespace Rcpp;

class WKTGeometryProvider: public GeometryProvider {
public:
  CharacterVector data;
  GEOSWKTReader *wkt_reader;
  size_t counter;

  WKTGeometryProvider(CharacterVector data) {
    this->data = data;
    this->counter = 0;
  }

  void init(GEOSContextHandle_t context) {
    this->context = context;
    this->wkt_reader = GEOSWKTReader_create_r(context);
  }

  GEOSGeometry* getNext() {
    GEOSGeometry* geometry = GEOSWKTReader_read_r(
      this->context,
      this->wkt_reader,
      this->data[this->counter]
    );
    this->counter = this->counter + 1;
    return geometry;
  }

  void finish() {
    GEOSWKTReader_destroy_r(this->context, this->wkt_reader);
  }

  size_t size() {
    return (this->data).size();
  }

};

#endif
