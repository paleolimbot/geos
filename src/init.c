
#include "libgeos.h"
#include <Rinternals.h>

/* generated by data-raw/make_callentries.R */
extern SEXP geos_c_read_wkt(SEXP input);
extern SEXP geos_c_write_wkt(SEXP input, SEXP includeZ, SEXP precision, SEXP trim);
extern SEXP geos_c_read_wkb(SEXP input);
extern SEXP geos_c_write_wkb(SEXP input, SEXP includeZ, SEXP includeSRID, SEXP endian);
extern SEXP geos_c_read_xy(SEXP x, SEXP y);
extern SEXP geos_c_write_xy(SEXP input);
extern SEXP geos_c_segment_intersection(SEXP Sax0, SEXP Say0, SEXP Sax1, SEXP Say1, SEXP Sbx0, SEXP Sby0, SEXP Sbx1, SEXP Sby1);
extern SEXP geos_c_orientation_index(SEXP SAx, SEXP SAy, SEXP SBx, SEXP SBy, SEXP SPx, SEXP SPy);
extern SEXP geos_c_area(SEXP geom);
extern SEXP geos_c_length(SEXP geom);
extern SEXP geos_c_x(SEXP geom);
extern SEXP geos_c_y(SEXP geom);
extern SEXP geos_c_z(SEXP geom);
extern SEXP geos_c_xmin(SEXP geom);
extern SEXP geos_c_ymin(SEXP geom);
extern SEXP geos_c_xmax(SEXP geom);
extern SEXP geos_c_ymax(SEXP geom);
extern SEXP geos_c_minimum_clearance(SEXP geom);
extern SEXP geos_c_init();
extern SEXP geos_c_version_runtime();
extern SEXP geos_c_version_build();

static const R_CallMethodDef CallEntries[] = {
  {"geos_c_read_wkt", (DL_FUNC) &geos_c_read_wkt, 1},
  {"geos_c_write_wkt", (DL_FUNC) &geos_c_write_wkt, 4},
  {"geos_c_read_wkb", (DL_FUNC) &geos_c_read_wkb, 1},
  {"geos_c_write_wkb", (DL_FUNC) &geos_c_write_wkb, 4},
  {"geos_c_read_xy", (DL_FUNC) &geos_c_read_xy, 2},
  {"geos_c_write_xy", (DL_FUNC) &geos_c_write_xy, 1},
  {"geos_c_segment_intersection", (DL_FUNC) &geos_c_segment_intersection, 8},
  {"geos_c_orientation_index", (DL_FUNC) &geos_c_orientation_index, 6},
  {"geos_c_area", (DL_FUNC) &geos_c_area, 1},
  {"geos_c_length", (DL_FUNC) &geos_c_length, 1},
  {"geos_c_x", (DL_FUNC) &geos_c_x, 1},
  {"geos_c_y", (DL_FUNC) &geos_c_y, 1},
  {"geos_c_z", (DL_FUNC) &geos_c_z, 1},
  {"geos_c_xmin", (DL_FUNC) &geos_c_xmin, 1},
  {"geos_c_ymin", (DL_FUNC) &geos_c_ymin, 1},
  {"geos_c_xmax", (DL_FUNC) &geos_c_xmax, 1},
  {"geos_c_ymax", (DL_FUNC) &geos_c_ymax, 1},
  {"geos_c_minimum_clearance", (DL_FUNC) &geos_c_minimum_clearance, 1},
  {"geos_c_init", (DL_FUNC) &geos_c_init, 0},
  {"geos_c_version_runtime", (DL_FUNC) &geos_c_version_runtime, 0},
  {"geos_c_version_build", (DL_FUNC) &geos_c_version_build, 0},
  {NULL, NULL, 0}
};
/* end generated by data-raw/make_callentries.R */

void R_init_geos(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

SEXP geos_c_init() {
  libgeos_init_api();
  return R_NilValue;
}

SEXP geos_c_version_runtime() {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(GEOSversion()));
  UNPROTECT(1);
  return out;
}

SEXP geos_c_version_build() {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(GEOS_CAPI_VERSION));
  UNPROTECT(1);
  return out;
}
