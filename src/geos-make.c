
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
