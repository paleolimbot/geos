
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

// data structure and callback used for the generic query
struct QueryResult {
  GEOSContextHandle_t handle;
  R_xlen_t i;
  GEOSGeometry* geometry;
  GEOSPreparedGeometry* prepared;
  SEXP geom;
  SEXP extra;
  int* indexList;
  R_xlen_t currentIndex;
};

// prepare and extra are needed for may_intersect, equals, and equals_exact
// which don't need prepared geometries and do need an extra parameter (tolerance
// for equals_exact)
SEXP strtree_query_base(SEXP treeExternalPtr, SEXP geom, GEOSQueryCallback callback,
                        int prepare, SEXP extra) {
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

  GEOS_INIT();

  struct QueryResult queryResult = {
    .handle = handle,
    .geometry = NULL,
    .prepared = NULL,
    .geom = geos_c_strtree_data(treeExternalPtr),
    .extra = extra,
    .indexList = INTEGER(tempItemResult),
    .currentIndex = 0
  };

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

    const GEOSPreparedGeometry* prepared;
    if (prepare) {
      prepared = GEOSPrepare_r(handle, geometry);
      GEOS_CHECK_GEOMETRY(prepared, i);
    } else {
      prepared = NULL;
    }

    // reset the result cache and set the item geometry
    // need to cast because GEOSPrepare_r() returns const
    queryResult.currentIndex = 0;
    queryResult.i = i;
    queryResult.geometry = geometry;
    queryResult.prepared = (GEOSPreparedGeometry*) prepared;

    GEOSSTRtree_query_r(handle, tree, geometry, callback, &queryResult);

    GEOSPreparedGeom_destroy_r(handle, prepared);

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

// for callbacks, 'item' is what was added to
// the index in geos_c_strtree_create(),
// which was a pointer to an R 1-based integer

// the may_intersect querier says yes to everything
void strtree_callback_may_intersect(void* item, void* userdata) {
  struct QueryResult* queryResult = (struct QueryResult*) userdata;
  int itemInt = *((int*) item);
  queryResult->indexList[queryResult->currentIndex] = itemInt;
  queryResult->currentIndex++;
}

SEXP geos_c_strtree_query(SEXP treeExternalPtr, SEXP geom) {
  // don't prepare
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_may_intersect, 0, R_NilValue);
}

// predicate callbacks only make sense for predicates that only return true
// if there's some kind of intersection (everything except disjoint)
// not currently considering exceptions, because calling Rf_error() here
// will longjmp and we're being called from C++ across a .so boundary
#define GEOS_STRTREE_CALLBACK(_func)                                           \
  struct QueryResult* queryResult = (struct QueryResult*) userdata;            \
  int itemInt = *((int*) item);                                                \
                                                                               \
  SEXP itemGeomPtr = VECTOR_ELT(queryResult->geom, itemInt - 1);               \
  GEOSGeometry* geometryIndex = (GEOSGeometry*) R_ExternalPtrAddr(itemGeomPtr);\
  if (geometryIndex == NULL) {                                                 \
    return;                                                                    \
  }                                                                            \
                                                                               \
  int resultCode = _func(                                                      \
    queryResult->handle,                                                       \
    queryResult->prepared,                                                     \
    geometryIndex                                                              \
  );                                                                           \
                                                                               \
  if (resultCode) {                                                            \
    queryResult->indexList[queryResult->currentIndex] = itemInt;               \
    queryResult->currentIndex++;                                               \
  }

void strtree_callback_touches(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedTouches_r)
}
SEXP geos_c_touches_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_touches, 1, R_NilValue);
}

void strtree_callback_intersects(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedIntersects_r)
}
SEXP geos_c_intersects_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_intersects, 1, R_NilValue);
}

void strtree_callback_crosses(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedCrosses_r)
}
SEXP geos_c_crosses_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_crosses, 1, R_NilValue);
}

void strtree_callback_within(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedWithin_r)
}
SEXP geos_c_within_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_within, 1, R_NilValue);
}

void strtree_callback_contains(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedContains_r)
}
SEXP geos_c_contains_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_contains, 1, R_NilValue);
}

void strtree_callback_contains_properly(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedContainsProperly_r)
}
SEXP geos_c_contains_properly_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_contains_properly, 1, R_NilValue);
}

void strtree_callback_overlaps(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedOverlaps_r)
}
SEXP geos_c_overlaps_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_overlaps, 1, R_NilValue);
}

void strtree_callback_covers(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedCovers_r)
}
SEXP geos_c_covers_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_covers, 1, R_NilValue);
}

void strtree_callback_covered_by(void* item, void* userdata) {
  GEOS_STRTREE_CALLBACK(GEOSPreparedCoveredBy_r)
}
SEXP geos_c_covered_by_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_covered_by, 1, R_NilValue);
}

// the equals querier is slightly different because there is no prepared equals
void strtree_callback_equals(void* item, void* userdata) {
  struct QueryResult* queryResult = (struct QueryResult*) userdata;
  int itemInt = *((int*) item);

  SEXP itemGeomPtr = VECTOR_ELT(queryResult->geom, itemInt - 1);
  GEOSGeometry* geometryIndex = (GEOSGeometry*) R_ExternalPtrAddr(itemGeomPtr);
  if (geometryIndex == NULL) {
    return;
  }

  int resultCode = GEOSEquals_r(
    queryResult->handle,
    queryResult->geometry,
    geometryIndex
  );

  if (resultCode) {
    queryResult->indexList[queryResult->currentIndex] = itemInt;
    queryResult->currentIndex++;
  }
}
SEXP geos_c_equals_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_equals, 0, R_NilValue);
}

// the equals_exact querier is slightly different because there is a parameter (tolerance)
void strtree_callback_equals_exact(void* item, void* userdata) {
  struct QueryResult* queryResult = (struct QueryResult*) userdata;
  int itemInt = *((int*) item);

  SEXP itemGeomPtr = VECTOR_ELT(queryResult->geom, itemInt - 1);
  GEOSGeometry* geometryIndex = (GEOSGeometry*) R_ExternalPtrAddr(itemGeomPtr);
  if (geometryIndex == NULL) {
    return;
  }

  double tolerance = REAL(queryResult->extra)[queryResult->i];

  int resultCode = GEOSEqualsExact_r(
    queryResult->handle,
    queryResult->geometry,
    geometryIndex,
    tolerance
  );

  if (resultCode) {
    queryResult->indexList[queryResult->currentIndex] = itemInt;
    queryResult->currentIndex++;
  }
}
SEXP geos_c_equals_exact_matrix(SEXP geom, SEXP treeExternalPtr, SEXP tolerance) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_equals_exact, 0, tolerance);
}

// convenience function for testing *any* predicate relation in a tree
// this could be made faster using a dedicated callback, but the indexed
// predicates are so fast that it's unlikely this is ever practically
// a problem
SEXP geos_c_predicate_any(SEXP matrixResult) {
  R_xlen_t size = Rf_length(matrixResult);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);

  SEXP item;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(matrixResult, i);
    if (item == R_NilValue) {
      pResult[i] = NA_LOGICAL;
    } else {
      pResult[i] = Rf_length(item) > 0;
    }
  }

  UNPROTECT(1); // result
  return result;
}
