#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "geos-common.h"
#include "libgeos.h"

#include <limits.h>
#include <stdint.h>

SEXP geos_c_basic_strtree_create(SEXP node_capacity_sexp) {
  int node_capacity = INTEGER(node_capacity_sexp)[0];
  GEOS_INIT();
  GEOSSTRtree* tree = GEOSSTRtree_create_r(handle, node_capacity);
  if (tree == NULL) {
    GEOS_ERROR("%s", "Failed to create GEOSSTRtree*()");  // # nocov
  }

  SEXP tree_size_sexp = PROTECT(Rf_ScalarInteger(0));
  SEXP tree_is_finalized = PROTECT(Rf_ScalarInteger(0));
  SEXP tree_xptr =
      PROTECT(geos_common_tree_xptr(tree, tree_size_sexp, tree_is_finalized));
  UNPROTECT(3);
  return tree_xptr;
}

SEXP geos_c_basic_strtree_size(SEXP tree_xptr) {
  int size = INTEGER(R_ExternalPtrTag(tree_xptr))[0];
  return Rf_ScalarInteger(size);
}

SEXP geos_c_basic_strtree_finalized(SEXP tree_xptr) {
  int is_finalized = INTEGER(R_ExternalPtrProtected(tree_xptr))[0];
  return Rf_ScalarLogical(is_finalized);
}

GEOSGeometry* dummy_geometry_from_extent(GEOSContextHandle_t handle, double xmin,
                                         double ymin, double xmax, double ymax) {
  GEOSCoordSequence* seq = GEOSCoordSeq_create_r(handle, 2, 2);
  if (seq == NULL) {
    Rf_error("error creating GEOSCoordSequence");
  }

  int return_code = GEOSCoordSeq_setXY_r(handle, seq, 0, xmin, ymin);
  if (return_code == 0) {
    GEOSCoordSeq_destroy_r(handle, seq);
    Rf_error("error creating GEOSCoordSequence");
  }

  return_code = GEOSCoordSeq_setXY_r(handle, seq, 1, xmax, ymax);
  if (return_code == 0) {
    GEOSCoordSeq_destroy_r(handle, seq);
    Rf_error("error creating GEOSCoordSequence");
  }

  GEOSGeometry* result = GEOSGeom_createLineString_r(handle, seq);
  if (result == NULL) {
    GEOSCoordSeq_destroy_r(handle, seq);
    Rf_error("error creating GEOSGeometry");
  }

  return result;
}

SEXP geos_c_basic_strtree_insert_rect(SEXP tree_xptr, SEXP xmin_sexp, SEXP ymin_sexp,
                                      SEXP xmax_sexp, SEXP ymax_sexp) {
  int is_finalized = INTEGER(R_ExternalPtrProtected(tree_xptr))[0];
  if (is_finalized) {
    Rf_error("Can't insert into a geos_basic_strtree() that has been queried");
  }

  GEOS_INIT();

  GEOSSTRtree* tree = (GEOSSTRtree*)R_ExternalPtrAddr(tree_xptr);
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

    if (xmin[i] > xmax[i] || ymin[i] > ymax[i] || R_IsNA(xmin[i]) || R_IsNA(ymin[i]) ||
        R_IsNA(xmax[i]) || R_IsNA(ymax[i])) {
      tree_size[0]++;
      continue;
    }

    geom = dummy_geometry_from_extent(handle, xmin[i], ymin[i], xmax[i], ymax[i]);
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

struct BasicQuery {
  int ix_;
  int* ix;
  int* itree;
  R_xlen_t size;
  R_xlen_t capacity;
  char has_error;
  int limit;
};

void basic_query_append(struct BasicQuery* query, int itree) {
  if (query->size >= query->capacity) {
    R_xlen_t new_capacity = query->size * 2;
    if (new_capacity < 1024) {
      new_capacity = 1024;
    }

    query->ix = (int*)realloc(query->ix, new_capacity * sizeof(int));
    query->itree = (int*)realloc(query->itree, new_capacity * sizeof(int));

    if (query->ix == NULL || query->itree == NULL) {
      query->has_error = 1;
      return;
    }

    query->capacity = new_capacity;
  }

  query->ix[query->size] = query->ix_;
  query->itree[query->size] = itree;
  query->size++;
  query->limit--;
}

void basic_query_finalize(SEXP query_xptr) {
  struct BasicQuery* query = (struct BasicQuery*)R_ExternalPtrAddr(query_xptr);
  if (query != NULL) {
    if (query->ix != NULL) free(query->ix);
    if (query->itree != NULL) free(query->itree);
    free(query);
    R_SetExternalPtrAddr(query_xptr, NULL);
  }
}

static void basic_query_callback(void* item, void* userdata) {
  uintptr_t item_value = (uintptr_t)item;
  struct BasicQuery* query = (struct BasicQuery*)userdata;
  if (query->has_error || query->limit <= 0) {
    return;
  }

  basic_query_append(query, item_value + 1);
}

SEXP geos_c_basic_strtree_query_geom(SEXP tree_xptr, SEXP xmin_sexp, SEXP ymin_sexp,
                                     SEXP xmax_sexp, SEXP ymax_sexp, SEXP limit_sexp,
                                     SEXP fill_sexp) {
  int limit = INTEGER(limit_sexp)[0];
  if (limit < 0) {
    limit = INT_MAX;
  }

  int fill = LOGICAL(fill_sexp)[0];

  SEXP is_finalized = PROTECT(R_ExternalPtrProtected(tree_xptr));
  INTEGER(is_finalized)[0] = 1;
  UNPROTECT(1);

  double* xmin = REAL(xmin_sexp);
  double* ymin = REAL(ymin_sexp);
  double* xmax = REAL(xmax_sexp);
  double* ymax = REAL(ymax_sexp);
  int n = Rf_length(xmin_sexp);

  GEOS_INIT();

  GEOSSTRtree* tree = (GEOSSTRtree*)R_ExternalPtrAddr(tree_xptr);
  if (tree == NULL) {
    Rf_error("External pointer (GEOSSTRtree) is not valid");
  }

  struct BasicQuery* query = (struct BasicQuery*)malloc(sizeof(struct BasicQuery));
  query->capacity = 0;
  query->size = 0;
  query->has_error = 0;
  query->limit = 0;
  query->ix = NULL;
  query->itree = NULL;
  query->ix_ = -1;

  SEXP query_shelter = PROTECT(R_MakeExternalPtr(query, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(query_shelter, &basic_query_finalize);

  GEOSGeometry* geom;
  for (int i = 0; i < n; i++) {
    if ((i % 1000) == 0) {
      R_CheckUserInterrupt();
    }

    geom = dummy_geometry_from_extent(handle, xmin[i], ymin[i], xmax[i], ymax[i]);

    query->ix_ = i + 1;
    query->has_error = 0;
    query->limit = limit;
    GEOSSTRtree_query_r(handle, tree, geom, &basic_query_callback, query);
    GEOSGeom_destroy_r(handle, geom);
    if (query->has_error) {
      Rf_error("Failed to allocate container for result indices [i = %ld]", (long)i + 1);
    }

    if (fill && query->limit > 0) {
      for (int j = 0; j < query->limit; j++) {
        basic_query_append(query, NA_INTEGER);
      }
    }
  }

  SEXP result_x = PROTECT(Rf_allocVector(INTSXP, query->size));
  SEXP result_tree = PROTECT(Rf_allocVector(INTSXP, query->size));
  if (query->size > 0) {
    memcpy(INTEGER(result_x), query->ix, query->size * sizeof(int));
    memcpy(INTEGER(result_tree), query->itree, query->size * sizeof(int));
  }

  basic_query_finalize(query_shelter);

  const char* names[] = {"x", "tree", ""};
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
  SET_VECTOR_ELT(result, 0, result_x);
  SET_VECTOR_ELT(result, 1, result_tree);
  UNPROTECT(4);
  return result;
}
