
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

SEXP geos_c_make_point(SEXP x, SEXP y, SEXP z) {
  R_xlen_t size = Rf_xlength(x);
  double* px = REAL(x);
  double* py = REAL(y);
  double* pz = REAL(z);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  GEOSGeometry* geometry;
  GEOSCoordSequence* seq;

  for (R_xlen_t i = 0; i < size; i++) {
    seq = NULL;
    if (ISNA(px[i]) && ISNA(py[i]) && ISNA(pz[i])) {
      geometry = GEOSGeom_createEmptyPoint_r(handle);
    } else if (ISNA(pz[i])) {
      seq = GEOSCoordSeq_create_r(handle, 1, 2);
      GEOSCoordSeq_setXY_r(handle, seq, 0, px[i], py[i]);
      geometry = GEOSGeom_createPoint_r(handle, seq);
    } else {
      seq = GEOSCoordSeq_create_r(handle, 1, 3);
      GEOSCoordSeq_setXYZ_r(handle, seq, 0, px[i], py[i], pz[i]);
      geometry = GEOSGeom_createPoint_r(handle, seq);
    }

    if (geometry == NULL) {
      // don't know how to make this fire
      // # nocov start
      if (seq != NULL) {
        GEOSCoordSeq_destroy_r(handle, seq);
      }

      UNPROTECT(1); // result
      GEOS_ERROR("[i=%d] ", i + 1);
      // # nocov end
    }

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometry));
  }

  GEOS_FINISH();
  UNPROTECT(1); // result
  return result;
}

// needed below to clean allocated GEOSGeometry* before error
void cleanup_geoms(GEOSContextHandle_t handle, GEOSGeometry** geoms, int nGeoms) {
  for (int i = 0; i < nGeoms; i++) {
    GEOSGeom_destroy_r(handle, geoms[i]);
  }
}

SEXP geos_c_make_collection(SEXP geom, SEXP typeId, SEXP featureLengths) {
  int* pLengths = INTEGER(featureLengths);
  int intTypeId = INTEGER(typeId)[0];

  R_xlen_t size = Rf_xlength(featureLengths);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();

  // the index in geom
  R_xlen_t iGeom = 0;

  // looping over featureLengths
  int featureLength;
  SEXP item;
  GEOSGeometry* itemGeometry;
  GEOSGeometry* collection;

  for (R_xlen_t i = 0; i < size; i++) {
    featureLength = pLengths[i];
    GEOSGeometry* geoms[featureLength];

    for (int j = 0; j < featureLength; j++) {
      item = VECTOR_ELT(geom, iGeom);
      if (item == R_NilValue) {
        cleanup_geoms(handle, geoms, j);
        GEOS_FINISH();
        UNPROTECT(1);
        Rf_error("[i=%d] Can't nest a missing geometry", iGeom);
      }

      itemGeometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
      if (itemGeometry == NULL) {
        cleanup_geoms(handle, geoms, j);
        GEOS_FINISH();
        UNPROTECT(1);
        Rf_error("[i=%d] External pointer is not valid", iGeom);
      }

      geoms[j] = GEOSGeom_clone_r(handle, itemGeometry);
      // Don't know how to make this fire
      if (geoms[j] == NULL) {
        cleanup_geoms(handle, geoms, j);  // # nocov;
        UNPROTECT(1); // # nocov
        GEOS_ERROR("[i=%d] ", iGeom); // # nocov
      }

      iGeom++;
    }

    collection = GEOSGeom_createCollection_r(handle, intTypeId, geoms, featureLength);
    if (collection == NULL) {
      cleanup_geoms(handle, geoms, featureLength);
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", iGeom);
    }

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(collection));
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}



SEXP geos_c_empty(SEXP typeId) {
  R_xlen_t size = Rf_xlength(typeId);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
  int* pTypeId = INTEGER(typeId);

  GEOS_INIT();
  GEOSGeometry* geometry;

  for (R_xlen_t i = 0; i < size; i++) {
    if (pTypeId[i] == NA_INTEGER) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    switch (pTypeId[i]) {
    case 1:
      geometry = GEOSGeom_createEmptyPoint_r(handle);
      break;
    case 2:
      geometry = GEOSGeom_createEmptyLineString_r(handle);
      break;
    case 3:
      geometry = GEOSGeom_createEmptyPolygon_r(handle);
      break;
    default:
      geometry = GEOSGeom_createEmptyCollection_r(handle, pTypeId[i]);
    break;
    }

    if (geometry == NULL) {
      UNPROTECT(1);
      GEOS_ERROR("[i=%d] ", i);
    }

    SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometry));
  }

  GEOS_FINISH();
  UNPROTECT(1);
  return result;
}
