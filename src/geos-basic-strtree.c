#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "libgeos.h"
#include "geos-common.h"

SEXP geos_c_basic_strtree_create(SEXP node_capacity_sexp) {
  int node_capacity = INTEGER(node_capacity_sexp)[0];
  GEOS_INIT();
  GEOSSTRtree* tree = GEOSSTRtree_create_r(handle, node_capacity);
  if (tree == NULL) {
    GEOS_ERROR("%s", "Failed to create GEOSSTRtree*()"); // # nocov
  }

  SEXP tree_size_sexp = PROTECT(Rf_ScalarInteger(0));
  SEXP tree_is_finalized = PROTECT(Rf_ScalarLogical(0));
  SEXP tree_xptr = PROTECT(geos_common_tree_xptr(tree, tree_size_sexp, tree_is_finalized));
  UNPROTECT(3);
  return tree_xptr;
}

SEXP geos_c_basic_strtree_size(SEXP tree_xptr) {
  int size = INTEGER(R_ExternalPtrTag(tree_xptr))[0];
  return Rf_ScalarInteger(size);
}

SEXP geos_c_basic_strtree_finalized(SEXP tree_xptr) {
  int is_finalized = LOGICAL(R_ExternalPtrProtected(tree_xptr))[0];
  return Rf_ScalarLogical(is_finalized);
}
