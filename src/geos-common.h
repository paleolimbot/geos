
#include <string.h>
#include "Rinternals.h"
#include "libgeos.h"

#define GEOS_ERROR_MESSAGE_BUFFER_SIZE 1024

// error handler that writes message to the char[1024] at userdata
void geos_r_error_handler(const char *message, void* userdata);

// macros to set up and tear down the GEOS error handling code
// hardcodes 'handle' as the GEOS handle type
#define GEOS_INIT()  \
  GEOSContextHandle_t handle = GEOS_init_r();  \
  char lastGEOSErrorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE];  \
  strcpy(lastGEOSErrorMessage, "(there was no GEOS error)");  \
  GEOSContext_setErrorMessageHandler_r(handle, &geos_r_error_handler, lastGEOSErrorMessage)\

// the error macro wraps Rf_error() but adds the error message at the end
// works for the frequent usage with exactly one arg to Rf_error()
#define GEOS_ERROR(msg, arg) Rf_error(strcat(msg, lastGEOSErrorMessage), arg)

// finishes handle
#define GEOS_FINISH() GEOS_finish_r(handle)
