
#include "libgeos.h"
#include <Rinternals.h>

SEXP c_geos_version() {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(GEOSversion()));
  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef CallEntries[] = {
  {"c_geos_version", (DL_FUNC) &c_geos_version, 0},
  {NULL, NULL, 0}
};

extern void R_init_geos(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  libgeos_init_api();
}
