
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
