
#include <string.h>
#include "Rinternals.h"
#include "libgeos.h"

// this leaves 200 characters of space for the prepending
// text for the error (which is always hard-coded)
#define GEOS_ERROR_MESSAGE_BUFFER_SIZE 1024
#define GEOS_ACTUAL_ERROR_MESSAGE_BUFFER_SIZE 1224

// error handler that writes message to the char[1024] at userdata
void geos_common_handle_error(const char *message, void* userdata);

// call GEOSGeom_destory_r(), finishing an externalptr
void geos_common_release_geometry(SEXP externalPtr);

// creates an externalptr with the appropriate finisher
SEXP geos_common_geometry_xptr(GEOSGeometry* geometry);

// macros to set up and tear down the GEOS error handling code
// hardcodes 'handle' as the GEOS handle type
#define GEOS_INIT()  \
  GEOSContextHandle_t handle = GEOS_init_r();  \
  char lastGEOSErrorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE];  \
  strcpy(lastGEOSErrorMessage, "(there was no GEOS error)");  \
  GEOSContext_setErrorMessageHandler_r(handle, &geos_common_handle_error, lastGEOSErrorMessage)\

// the error macro wraps Rf_error() but adds the error message at the end
// works for the frequent usage with exactly one arg to Rf_error()
// only works in the same function where GEOS_INIT() was used
#define GEOS_ERROR(msg, arg) GEOS_FINISH();  \
  char actualErrorMessage[GEOS_ACTUAL_ERROR_MESSAGE_BUFFER_SIZE];     \
  strcpy(actualErrorMessage, msg); \
  memcpy(&actualErrorMessage[strlen(msg)], lastGEOSErrorMessage, strlen(lastGEOSErrorMessage)); \
  actualErrorMessage[strlen(msg) + strlen(lastGEOSErrorMessage)] = '\0'; \
  Rf_error(actualErrorMessage, arg)

// finishes handle
#define GEOS_FINISH() GEOS_finish_r(handle)

// check geometry coming out of an externalptr
#define GEOS_CHECK_GEOMETRY(geometry, i)  \
  if (geometry == NULL) {   \
    Rf_error("External pointer is not valid [i=%d]", i + 1);  \
  }
