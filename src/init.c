
#include "libgeos.h"
#include <Rinternals.h>

extern SEXP c_geos_init() {
  libgeos_init_api();
  return R_NilValue;
}

extern SEXP c_geos_version_runtime() {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(GEOSversion()));
  UNPROTECT(1);
  return out;
}

extern SEXP c_geos_version_build() {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(GEOS_CAPI_VERSION));
  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef CallEntries[] = {
  {"c_geos_init", (DL_FUNC) &c_geos_init, 0},
  {"c_geos_version_runtime", (DL_FUNC) &c_geos_version_runtime, 0},
  {"c_geos_version_build", (DL_FUNC) &c_geos_version_build, 0},
  {NULL, NULL, 0}
};

void R_init_geos(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
