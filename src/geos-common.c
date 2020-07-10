
#include <string.h>
#include "geos-common.h"

void geos_common_handle_error(const char *message, void* userdata) {
  char* errorMessage = (char*) userdata;
  unsigned long messageChars = strlen(message);
  if (messageChars >= (GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1)) {
    // GEOS hands the error here with a max length of BUFSIZ, which is typically 1024
    // so this is unlikely to fire (was tested by reducing GEOS_ERROR_MESSAGE_BUFFER_SIZE
    // such that it is less than 1024)
    strncpy(errorMessage, message, GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1); // # nocov
    errorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1] = '\0'; // # nocov
  } else {
    strncpy(errorMessage, message, messageChars);
    errorMessage[messageChars] = '\0';
  }
}

void geos_common_release_geometry(SEXP externalPtr) {
  GEOSGeometry* geometry = (GEOSGeometry*) R_ExternalPtrAddr(externalPtr);
  // geometry should not be NULL, but R will crash if NULL is passed here
  // this can occur if this object is saved and reloaded, in which
  // case this function quietly does nothing
  if (geometry != NULL) {
    GEOSContextHandle_t handle = GEOS_init_r();
    GEOSGeom_destroy_r(handle, geometry);
    GEOS_finish_r(handle);
  }
}

SEXP geos_common_geometry_xptr(GEOSGeometry* geometry) {
  SEXP externalPtr = R_MakeExternalPtr((void *) geometry, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(externalPtr, geos_common_release_geometry, TRUE);
  return externalPtr;
}
