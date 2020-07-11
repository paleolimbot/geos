
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

// These functions are in the form _func(handle, geometry, double*)
// and return 0 on exception (except GEOSMinimumClearance_r, which
// returns 2 on exception)
#define GEOS_UNARY_REAL(_func, _errorValue)                     \
  R_xlen_t size = Rf_xlength(geom);                            \
  SEXP result = PROTECT(Rf_allocVector(REALSXP, size));        \
  double* pResult = REAL(result);                              \
                                                               \
  GEOS_INIT();                                                 \
                                                               \
  SEXP item;                                                   \
  GEOSGeometry* geometry;                                      \
  for (R_xlen_t i = 0; i < size; i++) {                        \
    item = VECTOR_ELT(geom, i);                                \
                                                               \
    if (item == R_NilValue) {                                  \
      pResult[i] = NA_REAL;                                    \
      continue;                                                \
    }                                                          \
                                                               \
    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);        \
    GEOS_CHECK_GEOMETRY(geometry, i);                          \
                                                               \
    int resultCode = _func(handle, geometry, &pResult[i]);     \
                                                               \
    if (resultCode == _errorValue) {                           \
      UNPROTECT(1);                                            \
      GEOS_ERROR("[i=%d] ", i);                                \
    }                                                          \
  }                                                            \
                                                               \
  GEOS_FINISH();                                               \
  UNPROTECT(1);                                                \
  return result;


// extractors
SEXP geos_c_area(SEXP geom) {
  GEOS_UNARY_REAL(GEOSArea_r, 0);
}

SEXP geos_c_length(SEXP geom) {
  GEOS_UNARY_REAL(GEOSLength_r, 0);
}

SEXP geos_c_x(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetX_r, 0);
}

SEXP geos_c_y(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetY_r, 0);
}

SEXP geos_c_z(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetZ_r, 0);
}

SEXP geos_c_xmin(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getXMin_r, 0);
}

SEXP geos_c_ymin(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getYMin_r, 0);
}

SEXP geos_c_xmax(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getXMax_r, 0);
}

SEXP geos_c_ymax(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getYMax_r, 0);
}

SEXP geos_c_minimum_clearance(SEXP geom) {
  GEOS_UNARY_REAL(GEOSMinimumClearance_r, 2);
}
