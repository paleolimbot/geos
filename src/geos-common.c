
#include <string.h>
#include "geos-common.h"

void geos_common_handle_error(const char *message, void* userdata) {
  char* errorMessage = (char*) userdata;
  unsigned long messageChars = strlen(message);
  if (messageChars >= (GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1)) {
    // GEOS hands the error here with a max length of BUFSIZ, which is typically 1024
    // so this is unlikely to fire (was tested by reducing GEOS_ERROR_MESSAGE_BUFFER_SIZE
    // such that it is less than 1024)
    memcpy(errorMessage, message, GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1); // # nocov
    errorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1] = '\0'; // # nocov
  } else {
    memcpy(errorMessage, message, messageChars);
    errorMessage[messageChars] = '\0';
  }
}

// initialize (actual value is set in R_init_geos())
GEOSContextHandle_t globalHandle = NULL;
char globalErrorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE];

void geos_common_release_geometry(SEXP externalPtr) {
  GEOSGeometry* geometry = (GEOSGeometry*) R_ExternalPtrAddr(externalPtr);

  // geometry should not be NULL, but R will crash if NULL is passed here
  // this can occur if this object is saved and reloaded, in which
  // case this function quietly does nothing.
  if ((geometry != NULL) && (globalHandle != NULL)) {
    GEOSGeom_destroy_r(globalHandle, geometry);
  } else if (geometry != NULL) {
    // in the unlikely event that the garbage collector runs after unload
    // create a handle just for this (most likely during development)
    GEOSContextHandle_t handle = GEOS_init_r(); // # nocov
    GEOSGeom_destroy_r(handle, geometry); // # nocov
    GEOS_finish_r(handle); // # nocov
  }
}

SEXP geos_common_geometry_xptr(GEOSGeometry* geometry) {
  SEXP externalPtr = R_MakeExternalPtr((void *) geometry, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(externalPtr, geos_common_release_geometry, TRUE);
  return externalPtr;
}

SEXP geos_common_child_geometry_xptr(const GEOSGeometry* geometry, SEXP parent) {
  return R_MakeExternalPtr((void *) geometry, parent, R_NilValue);
}

void geos_common_release_tree(SEXP externalPtr) {
  GEOSSTRtree* tree = (GEOSSTRtree*) R_ExternalPtrAddr(externalPtr);
  if (tree != NULL) {
    GEOSSTRtree_destroy_r(globalHandle, tree);
  }
}

SEXP geos_common_tree_xptr(GEOSSTRtree* tree, SEXP geom, SEXP indices) {
  SEXP externalPtr = R_MakeExternalPtr((void *) tree, geom, indices);
  R_RegisterCFinalizerEx(externalPtr, geos_common_release_tree, TRUE);
  return externalPtr;
}
