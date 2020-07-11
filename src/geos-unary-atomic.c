
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

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


SEXP geos_c_area(SEXP geom) {
  GEOS_UNARY_ATOMIC(GEOSArea_r, double, REALSXP, REAL, NA_REAL, 0);
}
