
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

void geos_common_release_geometry(SEXP externalPtr) {
  GEOSGeometry* geometry = (GEOSGeometry*) R_ExternalPtrAddr(externalPtr);
  // geometry should not be NULL, but R will crash if NULL is passed here
  // this can occur if this object is saved and reloaded, in which
  // case this function quietly does nothing
  if (geometry != NULL) {
    GEOSGeom_destroy_r(geos_gc_handle, geometry);
  }
}

SEXP geos_common_geometry_xptr(GEOSGeometry* geometry) {
  SEXP externalPtr = R_MakeExternalPtr((void *) geometry, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(externalPtr, geos_common_release_geometry, TRUE);
  return externalPtr;
}

void geos_common_release_tree(SEXP externalPtr) {
  GEOSSTRtree* tree = (GEOSSTRtree*) R_ExternalPtrAddr(externalPtr);
  if (tree != NULL) {
    GEOSSTRtree_destroy_r(geos_gc_handle, tree);
  }
}

SEXP geos_common_tree_xptr(GEOSSTRtree* tree, SEXP geom, SEXP indices) {
  SEXP externalPtr = R_MakeExternalPtr((void *) tree, geom, indices);
  R_RegisterCFinalizerEx(externalPtr, geos_common_release_tree, TRUE);
  return externalPtr;
}
