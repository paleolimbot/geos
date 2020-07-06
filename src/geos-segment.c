
#include "libgeos.h"
#include "Rinternals.h"

SEXP geos_c_segment_intersection(SEXP Sax0, SEXP Say0, SEXP Sax1, SEXP Say1,
                                 SEXP Sbx0, SEXP Sby0, SEXP Sbx1, SEXP Sby1) {
  R_xlen_t size = Rf_xlength(Sax0);
  SEXP resultX = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP resultY = PROTECT(Rf_allocVector(REALSXP, size));

  GEOSContextHandle_t handle = GEOS_init_r();
  int resultCode;

  double* ax0 = REAL(Sax0);
  double* ay0 = REAL(Say0);
  double* ax1 = REAL(Sax1);
  double* ay1 = REAL(Say1);
  double* bx0 = REAL(Sbx0);
  double* by0 = REAL(Sby0);
  double* bx1 = REAL(Sbx1);
  double* by1 = REAL(Sby1);
  double* cx = REAL(resultX);
  double* cy = REAL(resultY);

  for (R_xlen_t i = 0; i < size; i++) {
    if (R_FINITE(ax0[i]) && R_FINITE(ay0[i]) &&
        R_FINITE(ax1[i]) && R_FINITE(ay1[i]) &&
        R_FINITE(bx0[i]) && R_FINITE(by0[i]) &&
        R_FINITE(bx1[i]) && R_FINITE(by1[i])) {

      resultCode = GEOSSegmentIntersection_r(
        handle,
        ax0[i], ay0[i],
        ax1[i], ay1[i],
        bx0[i], by0[i],
        bx1[i], by1[i],
        &cx[i], &cy[i]
      );

      // returns 0 on error, -1 if segments do not intersect
      if (resultCode == 1) {
        REAL(resultX)[i] = cx[i];
        REAL(resultY)[i] = cy[i];
      } else if (resultCode == -1) {
        REAL(resultX)[i] = NAN;
        REAL(resultY)[i] = NAN;
      } else {
        Rf_error("Can't compute segment intersection [i=%d]", i);
      }

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

SEXP geos_c_orientation_index(SEXP SAx, SEXP SAy, SEXP SBx, SEXP SBy, SEXP SPx, SEXP SPy) {
  R_xlen_t size = Rf_xlength(SAx);
  SEXP result = PROTECT(Rf_allocVector(INTSXP, size));

  GEOSContextHandle_t handle = GEOS_init_r();

  double* Ax = REAL(SAx);
  double* Ay = REAL(SAy);
  double* Bx = REAL(SBx);
  double* By = REAL(SBy);
  double* Px = REAL(SPx);
  double* Py = REAL(SPy);
  int resultCode;

  for (R_xlen_t i = 0; i < size; i++) {
    if (R_FINITE(Ax[i]) && R_FINITE(Ay[i]) && R_FINITE(Bx[i]) && R_FINITE(By[i]) &&
        R_FINITE(Px[i]) && R_FINITE(Py[i])) {
      resultCode = GEOSOrientationIndex_r(
        handle,
        Ax[i], Ay[i], Bx[i], By[i],
        Px[i], Py[i]
      );

      // returns 2 on error
      if (resultCode == 2) {
        Rf_error("Can't compute orientation index [i=%d]", i);
      }

      INTEGER(result)[i] = resultCode;
    } else {
      INTEGER(result)[i] = NA_INTEGER;
    }
  }

  GEOS_finish_r(handle);
  UNPROTECT(1);
  return result;
}
