
#include "geos-common.h"
#include <string.h>

void geos_common_handle_error(const char* message, void* userdata) {
  char* errorMessage = (char*)userdata;
  unsigned long messageChars = strlen(message);
  if (messageChars >= (GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1)) {
    // GEOS hands the error here with a max length of BUFSIZ, which is typically 1024
    // so this is unlikely to fire (was tested by reducing GEOS_ERROR_MESSAGE_BUFFER_SIZE
    // such that it is less than 1024)
    memcpy(errorMessage, message, GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1);  // # nocov
    errorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE - 1] = '\0';            // # nocov
  } else {
    memcpy(errorMessage, message, messageChars);
    errorMessage[messageChars] = '\0';
  }
}

// initialize (actual value is set in R_init_geos())
GEOSContextHandle_t globalHandle = NULL;
char globalErrorMessage[GEOS_ERROR_MESSAGE_BUFFER_SIZE];

void geos_common_release_geometry(SEXP externalPtr) {
  // There may be a prepared version of the geometry stored in the Tag slot
  // that should be destroyed first
  SEXP prepExternalPtr = R_ExternalPtrTag(externalPtr);
  if (prepExternalPtr != R_NilValue) {
    const GEOSPreparedGeometry* prepared =
        (const GEOSPreparedGeometry*)R_ExternalPtrAddr(prepExternalPtr);
    if ((prepared != NULL) && (globalHandle != NULL)) {
      GEOSPreparedGeom_destroy_r(globalHandle, prepared);
    } else if (prepared != NULL) {
      GEOSContextHandle_t handle = GEOS_init_r();    // # nocov
      GEOSPreparedGeom_destroy_r(handle, prepared);  // # nocov
      GEOS_finish_r(handle);                         // # nocov
    }
  }

  GEOSGeometry* geometry = (GEOSGeometry*)R_ExternalPtrAddr(externalPtr);
  if ((geometry != NULL) && (globalHandle != NULL)) {
    GEOSGeom_destroy_r(globalHandle, geometry);
  } else if (geometry != NULL) {
    // in the unlikely event that the garbage collector runs after unload
    // create a handle just for this (most likely during development)
    GEOSContextHandle_t handle = GEOS_init_r();  // # nocov
    GEOSGeom_destroy_r(handle, geometry);        // # nocov
    GEOS_finish_r(handle);                       // # nocov
  }
}

SEXP geos_common_geometry_xptr(GEOSGeometry* geometry) {
  SEXP externalPtr = R_MakeExternalPtr((void*)geometry, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(externalPtr, geos_common_release_geometry, TRUE);
  return externalPtr;
}

// Use GEOS-style error message (set global + return NULL)
const GEOSPreparedGeometry* geos_common_geometry_prepared(SEXP externalPtr) {
  GEOSGeometry* geometry = (GEOSGeometry*)R_ExternalPtrAddr(externalPtr);

  SEXP tag = R_ExternalPtrTag(externalPtr);
  if (tag == R_NilValue) {
    const GEOSPreparedGeometry* prepared = GEOSPrepare_r(globalHandle, geometry);
    // don't register a finalizer because the finalizer of externalPtr will
    // destroy the prepared geom
    R_SetExternalPtrTag(externalPtr,
                        R_MakeExternalPtr((void*)prepared, R_NilValue, R_NilValue));
    return prepared;
  } else {
    return (const GEOSPreparedGeometry*)R_ExternalPtrAddr(tag);
  }
}

SEXP geos_common_child_geometry_xptr(const GEOSGeometry* geometry, SEXP parent) {
  return R_MakeExternalPtr((void*)geometry, parent, R_NilValue);
}

void geos_common_release_tree(SEXP externalPtr) {
  GEOSSTRtree* tree = (GEOSSTRtree*)R_ExternalPtrAddr(externalPtr);
  if (tree != NULL) {
    GEOSSTRtree_destroy_r(globalHandle, tree);
  }
}

SEXP geos_common_tree_xptr(GEOSSTRtree* tree, SEXP geom, SEXP indices) {
  SEXP externalPtr = R_MakeExternalPtr((void*)tree, geom, indices);
  R_RegisterCFinalizerEx(externalPtr, geos_common_release_tree, TRUE);
  return externalPtr;
}
