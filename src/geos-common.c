
#include <string.h>
#include "geos-common.h"

void geos_common_handle_error(const char *message, void* userdata) {
  char* errorMessage = (char*) userdata;
  unsigned long messageChars = strlen(message);
  if (messageChars >= (GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1)) {
    strncpy(errorMessage, message, GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1);
    errorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1] = '\0';
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
