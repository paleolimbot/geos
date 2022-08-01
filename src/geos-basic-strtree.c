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

GEOSGeometry* dummy_geometry_from_extent(GEOSContextHandle_t handle,
                                         double xmin, double ymin,
                                         double xmax, double ymax) {
    double xs[] = {xmin, xmax};
    double ys[] = {ymin, ymax};

    GEOSCoordSequence* seq = GEOSCoordSeq_copyFromArrays_r(
      handle,
      xs,
      ys,
      NULL,
      NULL,
      2
    );

    if (seq == NULL) {
      Rf_error("error creating GEOSCoordSequence");
    }

    GEOSGeometry* result = GEOSGeom_createLineString_r(handle, seq);
    if (result == NULL) {
      GEOSCoordSeq_destroy_r(handle, seq);
      Rf_error("error creating GEOSGeometry");
    }

    return result;
  }

SEXP geos_c_basic_strtree_insert_geom(SEXP tree_xptr, SEXP geom) {
  int is_finalized = LOGICAL(R_ExternalPtrProtected(tree_xptr))[0];
  if (is_finalized) {
    Rf_error("Can't insert into a geos_basic_strtree() that has been queried");
  }

  GEOS_INIT();

  GEOSSTRtree* tree = (GEOSSTRtree*) R_ExternalPtrAddr(tree_xptr);
  if (tree == NULL) {
    Rf_error("External pointer (GEOSSTRtree) is not valid");
  }

  int n = Rf_length(geom);

  int* tree_size = INTEGER(R_ExternalPtrTag(tree_xptr));
  int tree_size_start = tree_size[0];
  uintptr_t payload;
  SEXP item;
  GEOSGeometry* geom_item;

  for (R_xlen_t i = 0; i < n; i++) {
    if ((i % 1000) == 0) {
      R_CheckUserInterrupt();
    }

    item = VECTOR_ELT(geom, i);
    if (item == R_NilValue) {
      tree_size[0]++;
      continue;
    }

    geom_item = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geom_item, i);
    payload = tree_size[0];
    GEOSSTRtree_insert_r(handle, tree, geom_item, (void*)payload);
    tree_size[0]++;
  }

  SEXP result = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(result)[0] = tree_size_start + 1;
  INTEGER(result)[1] = n;
  UNPROTECT(1);
  return result;
}

SEXP geos_c_basic_strtree_insert_rect(SEXP tree_xptr, SEXP xmin_sexp, SEXP ymin_sexp,
                                      SEXP xmax_sexp, SEXP ymax_sexp) {
  int is_finalized = LOGICAL(R_ExternalPtrProtected(tree_xptr))[0];
  if (is_finalized) {
    Rf_error("Can't insert into a geos_basic_strtree() that has been queried");
  }

  GEOS_INIT();

  GEOSSTRtree* tree = (GEOSSTRtree*) R_ExternalPtrAddr(tree_xptr);
  if (tree == NULL) {
    Rf_error("External pointer (GEOSSTRtree) is not valid");
  }

  double* xmin = REAL(xmin_sexp);
  double* ymin = REAL(ymin_sexp);
  double* xmax = REAL(xmax_sexp);
  double* ymax = REAL(ymax_sexp);
  int n = Rf_length(xmin_sexp);

  int* tree_size = INTEGER(R_ExternalPtrTag(tree_xptr));
  int tree_size_start = tree_size[0];
  uintptr_t payload;
  GEOSGeometry* geom;

  for (R_xlen_t i = 0; i < n; i++) {
    if ((i % 1000) == 0) {
      R_CheckUserInterrupt();
    }

    if (xmin[i] > xmax[i] || ymin[i] > ymax[i] ||
        R_IsNA(xmin[i]) || R_IsNA(ymin[i]) || R_IsNA(xmax[i]) || R_IsNA(ymax[i])) {
      tree_size[0]++;
      continue;
    }

    geom = dummy_geometry_from_extent(
      handle,
      xmin[i], ymin[i],
      xmax[i], ymax[i]
    );
    payload = tree_size[0];
    GEOSSTRtree_insert_r(handle, tree, geom, (void*)payload);
    tree_size[0]++;
    GEOSGeom_destroy_r(handle, geom);
  }

  SEXP result = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(result)[0] = tree_size_start + 1;
  INTEGER(result)[1] = n;
  UNPROTECT(1);
  return result;
}
