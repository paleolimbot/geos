
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "libgeos.h"
#include "wk-v1.h"

int geos_wk_read_point(GEOSGeometry* g) {
  return 0;
}

int geos_wk_read_linestring(GEOSGeometry* g) {
  return 0;
}

int geos_wk_read_polygon(GEOSGeometry* g) {
  return 0;
}

int geos_wk_read_collection(GEOSGeometry* g) {
  return 0;
}

SEXP geos_wk_read_geos_geometry(SEXP geom, wk_handler_t* handler) {
  return R_NilValue;
}

SEXP geos_c_wk_read_geos_geometry(SEXP geom, SEXP handler_xptr) {
  return wk_handler_run_xptr(&geos_wk_read_geos_geometry, geom, handler_xptr);
}
