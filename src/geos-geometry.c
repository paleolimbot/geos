#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP geos_c_geos_geometry_is_null(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);

  for (R_xlen_t i = 0; i < size; i++) {
    pResult[i] = VECTOR_ELT(geom, i) == R_NilValue;
  }

  UNPROTECT(1);
  return result;
}

SEXP geos_c_geos_geometry_is_null_or_xptr(SEXP geom) {
  R_xlen_t size = Rf_xlength(geom);
  SEXP result = PROTECT(Rf_allocVector(LGLSXP, size));
  int* pResult = LOGICAL(result);
  int typeOf;
  for (R_xlen_t i = 0; i < size; i++) {
    typeOf = TYPEOF(VECTOR_ELT(geom, i));
    pResult[i] = (typeOf == NILSXP) || (typeOf == EXTPTRSXP);
  }

  UNPROTECT(1);
  return result;
}

