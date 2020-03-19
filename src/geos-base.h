
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

#endif
