
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

#include <stdio.h>

SEXP geos_c_strtree_create(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);

  GEOS_INIT();

  // create the tree object
  GEOSSTRtree* tree = GEOSSTRtree_create_r(handle, size);

  // also needed is a sequence of integers to which we can store
  // void* pointers, which are the 'hook' for GEOSSTRtree_insert_r()
  SEXP geomIndices = PROTECT(Rf_allocVector(INTSXP, size));
  int* pGeomIndices = INTEGER(geomIndices);

  SEXP item;
  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);
    if (item == R_NilValue) {
      GEOSSTRtree_destroy_r(handle, tree);
      GEOS_FINISH();
      UNPROTECT(1); // geomIndices
      Rf_error("Can't insert NULL into a geos_str_tree()");
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    // store the index of the geometry ("from whence it came")
    // store R 1-based index
    pGeomIndices[i] = i + 1;
    GEOSSTRtree_insert_r(handle, tree, geometry, &(pGeomIndices[i]));
  }

  // tree holds reference to geom to protect it (and the GEOSGeometry* pointers)
  // from garbage collection
  SEXP treeExternalPtr = geos_common_tree_xptr(tree, geom, geomIndices);
  GEOS_FINISH();
  UNPROTECT(1); // geomIndices
  return treeExternalPtr;
}

// re-extract the geometries used to construct the tree
SEXP geos_c_strtree_data(SEXP treeExternalPtr) {
  return R_ExternalPtrTag(treeExternalPtr);
}

// data structure and callback used for query
struct QueryResult {
  int* indexList;
  R_xlen_t currentIndex;
};

void callback_add_item(void* item, void* userdata) {
  struct QueryResult* queryResult = (struct QueryResult*) userdata;

  // 'item' is what was added to the index in geos_c_strtree_create(),
  // which was a pointer to an R 1-based integer
  int* itemInt = (int*) item;
  queryResult->indexList[queryResult->currentIndex] = *itemInt;
  queryResult->currentIndex++;
}

SEXP geos_c_strtree_query(SEXP treeExternalPtr, SEXP geom) {
  GEOSSTRtree* tree = (GEOSSTRtree*) R_ExternalPtrAddr(treeExternalPtr);
  if (tree == NULL) {
    Rf_error("External pointer (geos_strtree) is not valid");
  }

  // allocate the list() result
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  // allocate a temporary integer() along the length of the tree data
  // that will be used to contain the results while iterating
  SEXP tempItemResult =  PROTECT(
    Rf_allocVector(INTSXP, Rf_length(R_ExternalPtrProtected(treeExternalPtr)))
  );
  struct QueryResult queryResult = { .indexList = INTEGER(tempItemResult), .currentIndex = 0 };

  GEOS_INIT();

  SEXP item;
  SEXP itemResult;
  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    // reset the result cache
    queryResult.currentIndex = 0;
    GEOSSTRtree_query_r(handle, tree, geometry, &callback_add_item, &queryResult);

    // at this point, queryResult now holds the indices of potential intersectors to `geometry`
    // allocate a new vector with the appropriate length and copy the temporary result there
    itemResult = PROTECT(Rf_allocVector(INTSXP, queryResult.currentIndex));
    memcpy(INTEGER(itemResult), queryResult.indexList, queryResult.currentIndex * sizeof(int));

    // set result to this vector
    SET_VECTOR_ELT(result, i, itemResult);
    UNPROTECT(1); // itemResult
  }

  GEOS_FINISH();
  UNPROTECT(2); // result, tempItemResult
  return result;
}

