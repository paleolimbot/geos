
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

// using macros to stamp out these functions with minimal
// repetition
#define GEOS_UNARY_ATOMIC(_func, _scalar_type, _vec_type, _vec_ptr, _na_value, _result_error) \
  R_xlen_t size = Rf_xlength(geom);                            \
  SEXP result = PROTECT(Rf_allocVector(_vec_type, size));      \
  _scalar_type* pResult = _vec_ptr(result);                    \
                                                               \
  GEOS_INIT();                                                 \
                                                               \
  SEXP item;                                                   \
  GEOSGeometry* geometry;                                      \
  for (R_xlen_t i = 0; i < size; i++) {                        \
    item = VECTOR_ELT(geom, i);                                \
                                                               \
    if (item == R_NilValue) {                                  \
      pResult[i] = _na_value;                                  \
      continue;                                                \
    }                                                          \
                                                               \
    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);        \
    GEOS_CHECK_GEOMETRY(geometry, i);                          \
                                                               \
    int resultCode = _func(handle, geometry, &pResult[i]);     \
                                                               \
    if (resultCode == _result_error) {                         \
      UNPROTECT(1);                                            \
      GEOS_ERROR("[i=%d] ", i);                                \
    }                                                          \
  }                                                            \
                                                               \
  GEOS_FINISH();                                               \
  UNPROTECT(1);                                                \
  return result;

#define GEOS_UNARY_REAL(_func) GEOS_UNARY_ATOMIC(_func, double, REALSXP, REAL, NA_REAL, 0)

// extractors
SEXP geos_c_area(SEXP geom) {
  GEOS_UNARY_REAL(GEOSArea_r);
}

SEXP geos_c_length(SEXP geom) {
  GEOS_UNARY_REAL(GEOSLength_r);
}

SEXP geos_c_x(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetX_r);
}

SEXP geos_c_y(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetY_r);
}

SEXP geos_c_z(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeomGetZ_r);
}

SEXP geos_c_xmin(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getXMin_r);
}

SEXP geos_c_ymin(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getYMin_r);
}

SEXP geos_c_xmax(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getXMax_r);
}

SEXP geos_c_ymax(SEXP geom) {
  GEOS_UNARY_REAL(GEOSGeom_getYMax_r);
}
