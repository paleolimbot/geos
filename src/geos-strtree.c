
#include "Rinternals.h"
#include "geos-common.h"
#include "libgeos.h"

#include <stdio.h>

SEXP geos_c_strtree_create(SEXP geom, SEXP node_capacity) {
  R_xlen_t size = Rf_xlength(geom);
  int nodeCapacityInt = INTEGER(node_capacity)[0];
  GEOS_INIT();

  // create the tree object
  GEOSSTRtree* tree = GEOSSTRtree_create_r(handle, nodeCapacityInt);
  if (tree == NULL) {
    GEOS_ERROR("%s", "Failed to create GEOSSTRtree*()");  // # nocov
  }

  // also needed is a sequence of indices to which we can store
  // void* pointers, which are the 'hook' for GEOSSTRtree_insert_r()
  SEXP geomIndices = PROTECT(Rf_allocVector(REALSXP, size));
  double* pGeomIndices = REAL(geomIndices);

  SEXP item;
  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);
    if (item == R_NilValue) {
      GEOSSTRtree_destroy_r(handle, tree);
      UNPROTECT(1);  // geomIndices
      Rf_error("Can't insert NULL into a geos_str_tree()");
    }

    geometry = (GEOSGeometry*)R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    // store the index of the geometry ("from whence it came")
    // store R 1-based index
    pGeomIndices[i] = i + 1;
    GEOSSTRtree_insert_r(handle, tree, geometry, pGeomIndices + i);
  }

  // Each call to a query function will need to accumulate indices.
  // in the absence of std::vector(), we alloc swap space of the
  // maximum size that would be needed (Rf_xlength(geomIndices)). Another
  // option would be to implement a growable vector using realloc
  // beucase it's highly unlikely that any query will actually return
  // all the elements.
  SEXP geomIndicesSwap = PROTECT(Rf_allocVector(REALSXP, size));
  // Not initializing this array causes a valgrind error when an strtree
  // is serialized, because uninitialized valules are fed to the serializer
  memset(REAL(geomIndicesSwap), 0, Rf_xlength(geomIndicesSwap) * sizeof(double));

  // We only get two SEXPs to protect from garbage collection with an externalptr
  // and we'll used one of to protect `geom`. Use a list() to get around this
  // for the second one.
  SEXP externalPtrProt = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(externalPtrProt, 0, geomIndices);
  SET_VECTOR_ELT(externalPtrProt, 1, geomIndicesSwap);

  SEXP treeExternalPtr = geos_common_tree_xptr(tree, geom, externalPtrProt);
  UNPROTECT(3);  // geomIndices, geomIndicesSwap, externalPtrProt
  return treeExternalPtr;
}

// Re-extract the geometries used to construct the tree
// not necessary for querying but necessary for distance calculations,
// and in this form, probably not faster
SEXP geos_c_strtree_data(SEXP treeExternalPtr) {
  return R_ExternalPtrTag(treeExternalPtr);
}

// Extract the 1:n vector that holds the index "tag" for each item
static inline SEXP geos_strtree_indices(SEXP treeExternalPtr) {
  return VECTOR_ELT(R_ExternalPtrProtected(treeExternalPtr), 0);
}

// Extract the mutable swap space used to accumulate matching values of
// geos_strtree_indices() while accumulating indices. Another option would be
// to implement a growable vector that allocs on the C heap (because we can't
// longjmp from within a callback).
static inline SEXP geos_strtree_swap(SEXP treeExternalPtr) {
  return VECTOR_ELT(R_ExternalPtrProtected(treeExternalPtr), 1);
}

// data structure and callback used for the generic query
struct QueryResult {
  GEOSContextHandle_t handle;
  R_xlen_t i;
  GEOSGeometry* geometry;
  GEOSPreparedGeometry* prepared;
  SEXP geom;
  SEXP extra;
  double* indexList;
  R_xlen_t currentIndex;
};

// prepare and extra are needed for may_intersect, equals, and equals_exact
// which don't need prepared geometries and do need an extra parameter (tolerance
// for equals_exact)
SEXP strtree_query_base(SEXP treeExternalPtr, SEXP geom, GEOSQueryCallback callback,
                        int prepare, SEXP extra) {
  GEOSSTRtree* tree = (GEOSSTRtree*)R_ExternalPtrAddr(treeExternalPtr);
  if (tree == NULL) {
    UNPROTECT(1);
    Rf_error("External pointer (geos_strtree) is not valid");
  }

  // allocate the list() result
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  struct QueryResult queryResult = {.handle = handle,
                                    .geometry = NULL,
                                    .prepared = NULL,
                                    .geom = geos_c_strtree_data(treeExternalPtr),
                                    .extra = extra,
                                    .indexList = REAL(geos_strtree_swap(treeExternalPtr)),
                                    .currentIndex = 0};

  SEXP item;
  SEXP itemResult;
  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*)R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    const GEOSPreparedGeometry* prepared;
    if (prepare) {
      prepared = geos_common_geometry_prepared(item);
      if (prepared == NULL) {
        Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);
      }
    } else {
      prepared = NULL;
    }

    // reset the result cache and set the item geometry
    // need to cast because GEOSPrepare_r() returns const
    queryResult.currentIndex = 0;
    queryResult.i = i;
    queryResult.geometry = geometry;
    queryResult.prepared = (GEOSPreparedGeometry*)prepared;

    GEOSSTRtree_query_r(handle, tree, geometry, callback, &queryResult);

    // at this point, queryResult now holds the indices of potential intersectors to
    // `geometry` allocate a new vector with the appropriate length and copy the temporary
    // result there
    itemResult = PROTECT(Rf_allocVector(REALSXP, queryResult.currentIndex));
    memcpy(REAL(itemResult), queryResult.indexList,
           queryResult.currentIndex * sizeof(double));

    // set result to this vector
    SET_VECTOR_ELT(result, i, itemResult);
    UNPROTECT(1);  // itemResult
  }

  UNPROTECT(1);  // result
  return result;
}

// for callbacks, 'item' is what was added to
// the index in geos_c_strtree_create(),
// which was a pointer to an R 1-based integer

// the may_intersect querier says yes to everything
void strtree_callback_may_intersect(void* item, void* userdata) {
  struct QueryResult* queryResult = (struct QueryResult*)userdata;
  double itemIndex = *((double*)item);
  queryResult->indexList[queryResult->currentIndex] = itemIndex;
  queryResult->currentIndex++;
}

SEXP geos_c_strtree_query(SEXP treeExternalPtr, SEXP geom) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_may_intersect, 0,
                            R_NilValue);
}

// predicate callbacks only make sense for predicates that only return true
// if there's some kind of intersection (everything except disjoint)
// not currently considering exceptions, because calling Rf_error() here
// will longjmp and we're being called from C++ across a .so boundary
#define GEOS_STRTREE_CALLBACK(_func)                                                 \
  struct QueryResult* queryResult = (struct QueryResult*)userdata;                   \
  double itemIndex = *((double*)item);                                               \
                                                                                     \
  SEXP itemGeomPtr = VECTOR_ELT(queryResult->geom, itemIndex - 1);                   \
  GEOSGeometry* geometryIndex = (GEOSGeometry*)R_ExternalPtrAddr(itemGeomPtr);       \
  if (geometryIndex == NULL) {                                                       \
    return;                                                                          \
  }                                                                                  \
                                                                                     \
  int resultCode = _func(queryResult->handle, queryResult->prepared, geometryIndex); \
                                                                                     \
  if (resultCode) {                                                                  \
    queryResult->indexList[queryResult->currentIndex] = itemIndex;                   \
    queryResult->currentIndex++;                                                     \
  }

void strtree_callback_touches(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedTouches_r)} SEXP
    geos_c_touches_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_touches, 1,
                            R_NilValue);
}

void strtree_callback_intersects(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedIntersects_r)} SEXP
    geos_c_intersects_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_intersects, 1,
                            R_NilValue);
}

void strtree_callback_crosses(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedCrosses_r)} SEXP
    geos_c_crosses_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_crosses, 1,
                            R_NilValue);
}

void strtree_callback_within(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedWithin_r)} SEXP
    geos_c_within_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_within, 1,
                            R_NilValue);
}

void strtree_callback_contains(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedContains_r)} SEXP
    geos_c_contains_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_contains, 1,
                            R_NilValue);
}

void strtree_callback_contains_properly(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedContainsProperly_r)} SEXP
    geos_c_contains_properly_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_contains_properly, 1,
                            R_NilValue);
}

void strtree_callback_overlaps(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedOverlaps_r)} SEXP
    geos_c_overlaps_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_overlaps, 1,
                            R_NilValue);
}

void strtree_callback_covers(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedCovers_r)} SEXP
    geos_c_covers_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_covers, 1,
                            R_NilValue);
}

void strtree_callback_covered_by(void* item, void* userdata){
    GEOS_STRTREE_CALLBACK(GEOSPreparedCoveredBy_r)} SEXP
    geos_c_covered_by_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_covered_by, 1,
                            R_NilValue);
}

// the equals querier is slightly different because there is no prepared equals
// quiet approach to errors because this is called across a .so boundary
void strtree_callback_equals(void* item, void* userdata) {
  struct QueryResult* queryResult = (struct QueryResult*)userdata;
  double itemIndex = *((double*)item);

  SEXP itemGeomPtr = VECTOR_ELT(queryResult->geom, itemIndex - 1);
  GEOSGeometry* geometryIndex = (GEOSGeometry*)R_ExternalPtrAddr(itemGeomPtr);
  // don't know how this could happen, as the tree was already checked for
  // NULL (which could happen on save/reload)
  if (geometryIndex == NULL) {
    return;  // # nocov
  }

  int resultCode =
      GEOSEquals_r(queryResult->handle, queryResult->geometry, geometryIndex);

  if (resultCode) {
    queryResult->indexList[queryResult->currentIndex] = itemIndex;
    queryResult->currentIndex++;
  }
}

SEXP geos_c_equals_matrix(SEXP geom, SEXP treeExternalPtr) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_equals, 0,
                            R_NilValue);
}

// the equals_exact querier is slightly different because there is a parameter (tolerance)
// quiet approach to errors because this is called accross a .so boundary
void strtree_callback_equals_exact(void* item, void* userdata) {
  struct QueryResult* queryResult = (struct QueryResult*)userdata;
  double itemIndex = *((double*)item);

  SEXP itemGeomPtr = VECTOR_ELT(queryResult->geom, itemIndex - 1);
  GEOSGeometry* geometryIndex = (GEOSGeometry*)R_ExternalPtrAddr(itemGeomPtr);
  // don't know how this could happen, as the tree was already checked for
  // NULL (which could happen on save/reload)
  if (geometryIndex == NULL) {
    return;  // # nocov
  }

  double tolerance = REAL(queryResult->extra)[queryResult->i];

  int resultCode = GEOSEqualsExact_r(queryResult->handle, queryResult->geometry,
                                     geometryIndex, tolerance);

  if (resultCode) {
    queryResult->indexList[queryResult->currentIndex] = itemIndex;
    queryResult->currentIndex++;
  }
}

SEXP geos_c_equals_exact_matrix(SEXP geom, SEXP treeExternalPtr, SEXP tolerance) {
  return strtree_query_base(treeExternalPtr, geom, &strtree_callback_equals_exact, 0,
                            tolerance);
}

// convenience function for testing *any* predicate relation in a tree
// this could be made faster using a dedicated callback, but the indexed
// predicates are so fast that it's unlikely this is ever practically
// a problem
SEXP geos_c_predicate_any(SEXP matrixResult) {
  R_xlen_t size = Rf_xlength(matrixResult);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);

  SEXP item;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(matrixResult, i);
    if (item == R_NilValue) {
      pResult[i] = NA_LOGICAL;
    } else {
      pResult[i] = Rf_xlength(item) > 0;
    }
  }

  UNPROTECT(1);  // result
  return result;
}

// data structure used for the distance query
struct DistanceQueryInfo {
  GEOSContextHandle_t handle;
  SEXP treeData;
  SEXP extra;
};

// distance functions
SEXP geos_strtree_nearest_base(SEXP treeExternalPtr, SEXP geom,
                               GEOSDistanceCallback callback, SEXP extra) {
  GEOSSTRtree* tree = (GEOSSTRtree*)R_ExternalPtrAddr(treeExternalPtr);

  if (tree == NULL) {
    UNPROTECT(1);
    Rf_error("External pointer (geos_strtree) is not valid");
  }

  R_xlen_t treeSize = Rf_xlength(geos_strtree_indices(treeExternalPtr));

  // allocate the list() result
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(REALSXP, size));
  double* pResult = REAL(result);

  GEOS_INIT();

  struct DistanceQueryInfo distanceInfo = {
      .handle = handle, .treeData = geos_c_strtree_data(treeExternalPtr), .extra = extra};

  SEXP item;
  GEOSGeometry* geometry;
  double* itemResult;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);

    if (item == R_NilValue) {
      pResult[i] = NA_REAL;
      continue;
    }

    geometry = (GEOSGeometry*)R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    if (treeSize > 0) {
      itemResult = (double*)GEOSSTRtree_nearest_generic_r(
          handle, tree, geometry, geometry, callback, &distanceInfo);

      if (itemResult == NULL) {
        Rf_error("[%ld] %s", (long)i + 1, globalErrorMessage);
      }

      pResult[i] = *itemResult;
    } else {
      pResult[i] = NA_REAL;
    }
  }

  UNPROTECT(1);  // result
  return result;
}

int callback_distance_error(const void* item1, const void* item2, double* distance,
                            void* userdata) {
  return 0;
}

SEXP geos_c_nearest_error(SEXP geom, SEXP treeExternalPtr) {
  return geos_strtree_nearest_base(treeExternalPtr, geom, &callback_distance_error,
                                   R_NilValue);
}

#define GEOS_CALLBACK_DIST(_call)                                                     \
  struct DistanceQueryInfo* info = (struct DistanceQueryInfo*)userdata;               \
  SEXP treeGeomExternalPtr = VECTOR_ELT(info->treeData, (*((double*)item1)) - 1);     \
  GEOSGeometry* treeGeometry = (GEOSGeometry*)R_ExternalPtrAddr(treeGeomExternalPtr); \
  GEOSGeometry* itemGeometry = (GEOSGeometry*)item2;                                  \
  return _call;

int callback_distance(const void* item1, const void* item2, double* distance,
                      void* userdata) {
  GEOS_CALLBACK_DIST(GEOSDistance_r(info->handle, itemGeometry, treeGeometry, distance));
}
SEXP geos_c_nearest(SEXP geom, SEXP treeExternalPtr) {
  return geos_strtree_nearest_base(treeExternalPtr, geom, &callback_distance, R_NilValue);
}

int callback_distance_indexed(const void* item1, const void* item2, double* distance,
                              void* userdata) {
  GEOS_CALLBACK_DIST(
      GEOSDistanceIndexed_r(info->handle, itemGeometry, treeGeometry, distance));
}
SEXP geos_c_nearest_indexed(SEXP geom, SEXP treeExternalPtr) {
  return geos_strtree_nearest_base(treeExternalPtr, geom, &callback_distance_indexed,
                                   R_NilValue);
}

int callback_distance_hausdorff(const void* item1, const void* item2, double* distance,
                                void* userdata) {
  GEOS_CALLBACK_DIST(
      GEOSHausdorffDistance_r(info->handle, itemGeometry, treeGeometry, distance));
}
SEXP geos_c_nearest_hausdorff(SEXP geom, SEXP treeExternalPtr) {
  return geos_strtree_nearest_base(treeExternalPtr, geom, &callback_distance_hausdorff,
                                   R_NilValue);
}

int callback_distance_frechet(const void* item1, const void* item2, double* distance,
                              void* userdata) {
  GEOS_CALLBACK_DIST(
      GEOSFrechetDistance_r(info->handle, itemGeometry, treeGeometry, distance));
}
SEXP geos_c_nearest_frechet(SEXP geom, SEXP treeExternalPtr) {
  return geos_strtree_nearest_base(treeExternalPtr, geom, &callback_distance_frechet,
                                   R_NilValue);
}

int callback_distance_frechet_densify(const void* item1, const void* item2,
                                      double* distance, void* userdata) {
  GEOS_CALLBACK_DIST(GEOSFrechetDistanceDensify_r(
      info->handle, itemGeometry, treeGeometry, REAL(info->extra)[0], distance));
}
SEXP geos_c_nearest_frechet_densify(SEXP geom, SEXP treeExternalPtr, SEXP densify) {
  return geos_strtree_nearest_base(treeExternalPtr, geom,
                                   &callback_distance_frechet_densify, densify);
}

int callback_distance_hausdorff_densify(const void* item1, const void* item2,
                                        double* distance, void* userdata) {
  GEOS_CALLBACK_DIST(GEOSHausdorffDistanceDensify_r(
      info->handle, itemGeometry, treeGeometry, REAL(info->extra)[0], distance));
}
SEXP geos_c_nearest_hausdorff_densify(SEXP geom, SEXP treeExternalPtr, SEXP densify) {
  return geos_strtree_nearest_base(treeExternalPtr, geom,
                                   &callback_distance_hausdorff_densify, densify);
}
