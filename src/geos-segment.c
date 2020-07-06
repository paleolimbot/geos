
#include "libgeos.h"
#include "Rinternals.h"

SEXP geos_c_segment_intersection(SEXP ax0, SEXP ay0, SEXP ax1, SEXP ay1,
                                 SEXP bx0, SEXP by0, SEXP bx1, SEXP by1) {
  R_xlen_t size = Rf_xlength(ax0);
  SEXP resultX = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP resultY = PROTECT(Rf_allocVector(REALSXP, size));

  GEOSContextHandle_t handle = GEOS_init_r();
  double cx, cy;
  int resultCode;

  for (R_xlen_t i = 0; i < size; i++) {
    resultCode = GEOSSegmentIntersection_r(
      handle,
      REAL(ax0)[i], REAL(ay0)[i],
      REAL(ax1)[i], REAL(ay1)[i],
      REAL(bx0)[i], REAL(by0)[i],
      REAL(bx1)[i], REAL(by1)[i],
      &cx, &cy
    );

    // returns 0 on error, but this includes nan/inf
    // for which we want to return NA anyway
    // would probably be faster to check finite above
    if (resultCode == 1) {
      REAL(resultX)[i] = cx;
      REAL(resultY)[i] = cy;
    } else {
      REAL(resultX)[i] = NA_REAL;
      REAL(resultY)[i] = NA_REAL;
    }
  }

  GEOS_finish_r(handle);

  // transfer protection responsibility to result
  UNPROTECT(2);
  const char* names[] = {"x", "y", ""};
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
  SET_VECTOR_ELT(result, 0, resultX);
  SET_VECTOR_ELT(result, 1, resultY);
  UNPROTECT(1);
  return result;
}

SEXP geos_c_orientation_index(SEXP Ax, SEXP Ay, SEXP Bx, SEXP By, SEXP Px, SEXP Py) {
  R_xlen_t size = Rf_xlength(Ax);
  SEXP result = PROTECT(Rf_allocVector(INTSXP, size));

  GEOSContextHandle_t handle = GEOS_init_r();

  for (R_xlen_t i = 0; i < size; i++) {
    int resultCode = GEOSOrientationIndex_r(
      handle,
      REAL(Ax)[i], REAL(Ay)[i], REAL(Bx)[i], REAL(By)[i],
      REAL(Px)[i], REAL(Py)[i]
    );

    // returns 2 on error, but this includes nan/inf
    // for which we want to return NA anyway
    // would probably be faster to check finite above
    if (resultCode == 2) {
      INTEGER(result)[i] = NA_INTEGER;
    } else {
      INTEGER(result)[i] = resultCode;
    }
  }

  GEOS_finish_r(handle);
  UNPROTECT(1);
  return result;
}
