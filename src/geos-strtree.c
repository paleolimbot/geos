
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"


SEXP geos_c_strtree_create(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);

  GEOS_INIT();

  // create the tree object
  GEOSSTRtree* tree = GEOSSTRtree_create_r(handle, size);

  SEXP item;
  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(geom, i);
    if (item == R_NilValue) {
      GEOSSTRtree_destroy_r(handle, tree);
      GEOS_FINISH();
      Rf_error("Can't insert NULL into a geos_str_tree()");
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    // store the index of the geometry ("from whence it came")
    // by casting to a void pointer rather than allocating a
    // vector of integer indices
    GEOSSTRtree_insert_r(handle, tree, geometry, (void*) i);
  }

  // tree holds reference to geom to protect it (and the GEOSGeometry* pointers)
  // from garbage collection
  SEXP treeExternalPtr = geos_common_tree_xptr(tree, geom);
  GEOS_FINISH();
  return treeExternalPtr;
}

// needed for the print method
SEXP geos_c_strtree_data(SEXP tree) {
  return R_ExternalPtrProtected(tree);
}
