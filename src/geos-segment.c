
#include "libgeos.h"
#include "Rinternals.h"

SEXP geos_c_segment_intersection(SEXP ax0, SEXP ay0, SEXP ax1, SEXP ay1,
                                 SEXP bx0, SEXP by0, SEXP bx1, SEXP by1) {
  R_xlen_t size = Rf_xlength(ax0);
  SEXP resultX = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP resultY = PROTECT(Rf_allocVector(REALSXP, size));

  GEOSContextHandle_t handle = GEOS_init_r();
  double ax0i, ay0i, ax1i, ay1i, bx0i, by0i, bx1i, by1i, cxi, cyi;
  int resultCode;

  for (R_xlen_t i = 0; i < size; i++) {
    ax0i = REAL(ax0)[i]; ay0i = REAL(ay0)[i];
    ax1i = REAL(ax1)[i]; ay1i = REAL(ay1)[i];
    bx0i = REAL(bx0)[i]; by0i = REAL(by0)[i];
    bx1i = REAL(bx1)[i], by1i = REAL(by1)[i];

    if (R_FINITE(ax0i) && R_FINITE(ay0i) &&
        R_FINITE(ax1i) && R_FINITE(ay1i) &&
        R_FINITE(bx0i) && R_FINITE(by0i) &&
        R_FINITE(bx1i) && R_FINITE(by1i)) {

      resultCode = GEOSSegmentIntersection_r(
        handle,
        ax0i, ay0i,
        ax1i, ay1i,
        bx0i, by0i,
        bx1i, by1i,
        &cxi, &cyi
      );

      // returns 0 on error, -1 if segments do not intersect
      if (resultCode == 1) {
        REAL(resultX)[i] = cxi;
        REAL(resultY)[i] = cyi;
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

SEXP geos_c_orientation_index(SEXP Ax, SEXP Ay, SEXP Bx, SEXP By, SEXP Px, SEXP Py) {
  R_xlen_t size = Rf_xlength(Ax);
  SEXP result = PROTECT(Rf_allocVector(INTSXP, size));

  GEOSContextHandle_t handle = GEOS_init_r();

  double Axi, Ayi, Bxi, Byi, Pxi, Pyi;
  int resultCode;

  for (R_xlen_t i = 0; i < size; i++) {
    Axi = REAL(Ax)[i];
    Ayi = REAL(Ay)[i];
    Bxi = REAL(Bx)[i];
    Byi = REAL(By)[i];
    Pxi = REAL(Px)[i];
    Pyi = REAL(Py)[i];

    if (R_FINITE(Axi) && R_FINITE(Ayi) && R_FINITE(Bxi) && R_FINITE(Byi) &&
        R_FINITE(Pxi) && R_FINITE(Pyi)) {
      resultCode = GEOSOrientationIndex_r(
        handle,
        Axi, Ayi, Bxi, Byi,
        Pxi, Pyi
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
