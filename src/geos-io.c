
#include "libgeos.h"
#include "geos-common.h"
#include "Rinternals.h"

SEXP geos_c_read_wkt(SEXP input) {
  R_xlen_t size = Rf_xlength(input);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();
  GEOSWKTReader* reader = GEOSWKTReader_create_r(handle);

  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    if (STRING_ELT(input, i) == NA_STRING) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = GEOSWKTReader_read_r(handle, reader, CHAR(STRING_ELT(input, i)));

    // returns NULL on error
    if (geometry == NULL) {
      GEOSWKTReader_destroy_r(handle, reader);
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometry));
    }
  }

  GEOSWKTReader_destroy_r(handle, reader);
    UNPROTECT(1); // result
  return result;
}

SEXP geos_c_write_wkt(SEXP input, SEXP includeZ, SEXP precision, SEXP trim) {
  R_xlen_t size = Rf_xlength(input);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));

  GEOS_INIT();
  GEOSWKTWriter* writer = GEOSWKTWriter_create_r(handle);
  GEOSWKTWriter_setTrim_r(handle, writer, LOGICAL(trim)[0]);
  GEOSWKTWriter_setRoundingPrecision_r(handle, writer, INTEGER(precision)[0]);

  if (LOGICAL(includeZ)[0]) {
    GEOSWKTWriter_setOutputDimension_r(handle, writer, 3);
  } else {
    GEOSWKTWriter_setOutputDimension_r(handle, writer, 2);
  }

  SEXP item;
  GEOSGeometry* geometry;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(input, i);

    if (item == R_NilValue) {
      SET_STRING_ELT(result, i, NA_STRING);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    if (geometry == NULL) {
      GEOSWKTWriter_destroy_r(handle, writer);
      GEOS_CHECK_GEOMETRY(geometry, i);
    }

    char* output = GEOSWKTWriter_write_r(handle, writer, geometry);
    if (output == NULL) {
      // don't know how to make this occur
      GEOSWKTWriter_destroy_r(handle, writer); // # nocov
      Rf_error("[%d] %s", i + 1, globalErrorMessage); // # nocov
    }

    SET_STRING_ELT(result, i, Rf_mkChar(output));
    GEOSFree_r(handle, output);
  }

  GEOSWKTWriter_destroy_r(handle, writer);
    UNPROTECT(1); // result
  return result;
}

SEXP geos_c_read_wkb(SEXP input) {
  R_xlen_t size = Rf_xlength(input);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();
  GEOSWKBReader* reader = GEOSWKBReader_create_r(handle);

  GEOSGeometry* geometry;
  SEXP item;
  R_xlen_t itemSize;

  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(input, i);
    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    itemSize = Rf_xlength(item);
    geometry = GEOSWKBReader_read_r(handle, reader, RAW(item), itemSize);

    // returns NULL on error
    if (geometry == NULL) {
      GEOSWKBReader_destroy_r(handle, reader);
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometry));
    }
  }

  GEOSWKBReader_destroy_r(handle, reader);
    UNPROTECT(1);
  return result;
}

SEXP geos_c_write_wkb(SEXP input, SEXP includeZ, SEXP includeSRID, SEXP endian, SEXP flavor) {
  R_xlen_t size = Rf_xlength(input);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();
  GEOSWKBWriter* writer = GEOSWKBWriter_create_r(handle);
  GEOSWKBWriter_setByteOrder_r(handle, writer, INTEGER(endian)[0]);

  if (LOGICAL(includeZ)[0]) {
    GEOSWKBWriter_setOutputDimension_r(handle, writer, 3);
  } else {
    GEOSWKBWriter_setOutputDimension_r(handle, writer, 2);
  }

  if (LOGICAL(includeSRID)[0]) {
    GEOSWKBWriter_setIncludeSRID_r(handle, writer, 1);
  } else {
    GEOSWKBWriter_setIncludeSRID_r(handle, writer, 0);
  }

  int flavor_default = 1;
  int flavor_int = INTEGER(flavor)[0];

  // in GEOS 3.10.0 the constants required to specify ISO vs EWKB
  // are swapped from those noted by the enum in the C API
  if (libgeos_version_int() == LIBGEOS_VERSION_INT(3, 10, 0)) {
    flavor_default = 2;

    if (flavor_int == 2) {
      flavor_int = 1;
    } else if (flavor_int == 1) {
      flavor_int = 2;
    }
  }

  if (flavor_int != flavor_default) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 10, 0)
    if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 10, 0)) {
      ERROR_OLD_LIBGEOS("GEOSWKBWriter_setFlavor_r()", "3.10.0");
    }

    GEOSWKBWriter_setFlavor_r(handle, writer, flavor_int);
#else
    ERROR_OLD_LIBGEOS_BUILD("GEOSWKBWriter_setFlavor_r()", "3.10.0");
#endif
  }

  SEXP item;
  GEOSGeometry* geometry;
  size_t itemSize;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(input, i);

    if (item == R_NilValue) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    if (geometry == NULL) {
      GEOSWKBWriter_destroy_r(handle, writer);
      GEOS_CHECK_GEOMETRY(geometry, i);
    }

    unsigned char* wkbPtr = GEOSWKBWriter_write_r(handle, writer, geometry, &itemSize);
    // returns NULL on error (e.g., when trying to write an empty point)
    if (wkbPtr == NULL) {
      GEOSWKBWriter_destroy_r(handle, writer);
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    }

    SEXP itemWKB = PROTECT(Rf_allocVector(RAWSXP, itemSize));
    memcpy(RAW(itemWKB), wkbPtr, itemSize);
    GEOSFree_r(handle, wkbPtr);
    SET_VECTOR_ELT(result, i, itemWKB);
    UNPROTECT(1); // itemWKB
  }

  GEOSWKBWriter_destroy_r(handle, writer);
  UNPROTECT(1); // result
  return result;
}

SEXP geos_c_read_hex(SEXP input) {
  R_xlen_t size = Rf_xlength(input);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

  GEOS_INIT();
  GEOSWKBReader* reader = GEOSWKBReader_create_r(handle);

  GEOSGeometry* geometry;
  SEXP item;
  char* itemChars;

  for (R_xlen_t i = 0; i < size; i++) {
    item = STRING_ELT(input, i);
    if (item == NA_STRING) {
      SET_VECTOR_ELT(result, i, R_NilValue);
      continue;
    }

    itemChars = (char*) CHAR(item);
    geometry = GEOSWKBReader_readHEX_r(handle, reader, (unsigned char*) itemChars, strlen(itemChars));

    // returns NULL on error
    if (geometry == NULL) {
      GEOSWKBReader_destroy_r(handle, reader);
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    } else {
      SET_VECTOR_ELT(result, i, geos_common_geometry_xptr(geometry));
    }
  }

  GEOSWKBReader_destroy_r(handle, reader);
    UNPROTECT(1);
  return result;
}

SEXP geos_c_write_hex(SEXP input, SEXP includeZ, SEXP includeSRID, SEXP endian, SEXP flavor) {
  R_xlen_t size = Rf_xlength(input);
  SEXP result = PROTECT(Rf_allocVector(STRSXP, size));

  GEOS_INIT();
  GEOSWKBWriter* writer = GEOSWKBWriter_create_r(handle);
  GEOSWKBWriter_setByteOrder_r(handle, writer, INTEGER(endian)[0]);

  if (LOGICAL(includeZ)[0]) {
    GEOSWKBWriter_setOutputDimension_r(handle, writer, 3);
  } else {
    GEOSWKBWriter_setOutputDimension_r(handle, writer, 2);
  }

  if (LOGICAL(includeSRID)[0]) {
    GEOSWKBWriter_setIncludeSRID_r(handle, writer, 1);
  } else {
    GEOSWKBWriter_setIncludeSRID_r(handle, writer, 0);
  }

  int flavor_default = 1;
  int flavor_int = INTEGER(flavor)[0];

  // in GEOS 3.10.0 the constants required to specify ISO vs EWKB
  // are swapped from those noted by the enum in the C API
  if (libgeos_version_int() == LIBGEOS_VERSION_INT(3, 10, 0)) {
    flavor_default = 2;

    if (flavor_int == 2) {
      flavor_int = 1;
    } else if (flavor_int == 1) {
      flavor_int = 2;
    }
  }

  if (flavor_int != flavor_default) {
#if LIBGEOS_VERSION_COMPILE_INT >= LIBGEOS_VERSION_INT(3, 10, 0)
    if (libgeos_version_int() < LIBGEOS_VERSION_INT(3, 10, 0)) {
      ERROR_OLD_LIBGEOS("GEOSWKBWriter_setFlavor_r()", "3.10.0");
    }

    GEOSWKBWriter_setFlavor_r(handle, writer, flavor_int);
#else
    ERROR_OLD_LIBGEOS_BUILD("GEOSWKBWriter_setFlavor_r()", "3.10.0");
#endif
  }

  SEXP item;
  GEOSGeometry* geometry;
  size_t itemSize;
  unsigned char* itemChars;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(input, i);

    if (item == R_NilValue) {
      SET_STRING_ELT(result, i, NA_STRING);
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    if (geometry == NULL) {
      GEOSWKBWriter_destroy_r(handle, writer);
      GEOS_CHECK_GEOMETRY(geometry, i);
    }

    itemChars = GEOSWKBWriter_writeHEX_r(handle, writer, geometry, &itemSize);
    // returns NULL on error (e.g., when trying to write an empty point)
    if (itemChars == NULL) {
      GEOSWKBWriter_destroy_r(handle, writer);
      Rf_error("[%d] %s", i + 1, globalErrorMessage);
    }

    SET_STRING_ELT(result, i, Rf_mkCharLen((const char*) itemChars, itemSize));
    GEOSFree_r(handle, itemChars);
  }

  GEOSWKBWriter_destroy_r(handle, writer);
    UNPROTECT(1); // result
  return result;
}

SEXP geos_c_write_xy(SEXP input) {
  R_xlen_t size = Rf_xlength(input);
  SEXP resultX = PROTECT(Rf_allocVector(REALSXP, size));
  SEXP resultY = PROTECT(Rf_allocVector(REALSXP, size));
  double* px = REAL(resultX);
  double* py = REAL(resultY);

  GEOS_INIT();

  SEXP item;
  GEOSGeometry* geometry;
  int codeX, codeY;
  for (R_xlen_t i = 0; i < size; i++) {
    item = VECTOR_ELT(input, i);

    if (item == R_NilValue) {
      px[i] = NA_REAL;
      py[i] = NA_REAL;
      continue;
    }

    geometry = (GEOSGeometry*) R_ExternalPtrAddr(item);
    GEOS_CHECK_GEOMETRY(geometry, i);

    if (GEOSisEmpty_r(handle, geometry)) {
      px[i] = NA_REAL;
      py[i] = NA_REAL;
    } else {
      codeX = GEOSGeomGetX_r(handle, geometry, &px[i]);
      codeY = GEOSGeomGetY_r(handle, geometry, &py[i]);
      if (codeX == 0 || codeY == 0) {
        // e.g., geometry is not a point
        Rf_error("[%d] %s", i + 1, globalErrorMessage);
      }
    }
  }


  const char* names[] = {"x", "y", ""};
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
  SET_VECTOR_ELT(result, 0, resultX);
  SET_VECTOR_ELT(result, 1, resultY);
  UNPROTECT(3); // resultX, resultY, result
  return result;
}
