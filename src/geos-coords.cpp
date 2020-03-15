
#include "geos-coords.h"
#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

List geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int type = GEOSGeomTypeId_r(context, geometry);
  List geoTbl;

  if (type == GEOSGeomTypes::GEOS_POINT) {
    geoTbl = simple_geometry_to_geo_tbl(context, geometry, feature, "geo_tbl_point");

  } else if(type == GEOSGeomTypes::GEOS_LINESTRING) {
    geoTbl = simple_geometry_to_geo_tbl(context, geometry, feature, "geo_tbl_linestring");

  } else if(type == GEOSGeomTypes::GEOS_POLYGON) {
    geoTbl = polygon_to_geo_tbl(context, geometry, feature);

  } else if(type == GEOSGeomTypes::GEOS_MULTIPOINT) {
    CharacterVector cls = CharacterVector::create("geo_tbl_multipoint", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");

    // multipoint doesn't need  the "part" field
    List tblBase = simple_multi_geometry_to_geo_tbl(context, geometry, feature, "geo_tbl_multipoint");

    geoTbl = List::create(
      _["xy"] = tblBase["xy"],
      _["feature"] = tblBase["feature"]
    );
    geoTbl.attr("class") = cls;

  } else if(type == GEOSGeomTypes::GEOS_MULTILINESTRING) {
    geoTbl = simple_multi_geometry_to_geo_tbl(context, geometry, feature, "geo_tbl_multilinestring");

  } else if(type == GEOSGeomTypes::GEOS_MULTIPOLYGON) {
    stop("Not implemented multipolygon");

  } else {
    stop("Can only convert point, linestring, polygon, and multi- variants to a geo_tbl");
  }

  return geoTbl;
}

List polygon_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int nInteriorRings = GEOSGetNumInteriorRings_r(context, geometry);
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

  NumericVector xVec(nCoordinates);
  NumericVector yVec(nCoordinates);
  IntegerVector piece(nCoordinates);

  unsigned int offset = 0;
  const GEOSGeometry* exterior = GEOSGetExteriorRing_r(context, geometry);
  offset += write_simple_geometry(context, exterior, xVec, yVec, piece, 1, offset);

  for(int i=0; i<nInteriorRings; i++) {
    const GEOSGeometry* ring = GEOSGetInteriorRingN_r(context, geometry, i);
    offset += write_simple_geometry(context, ring, xVec, yVec, piece, 2 + i, offset);
  }

  return new_geo_tbl(xVec, yVec, piece, feature, "geo_tbl_polygon");
}

List simple_multi_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry,
                                      int feature, const char* tblClass) {
  int nParts = GEOSGetNumGeometries_r(context, geometry);
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

  NumericVector xVec(nCoordinates);
  NumericVector yVec(nCoordinates);
  IntegerVector partVec(nCoordinates);

  unsigned int offset = 0;

  for(int i=0; i<nParts; i++) {
    const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
    offset += write_simple_geometry(context, part, xVec, yVec, partVec, i+1, offset);
  }

  List output = new_geo_tbl(xVec, yVec, feature, tblClass);
  output.push_back(partVec, "part");
  output.attr("class") = CharacterVector::create(tblClass, "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
  return output;
}

List simple_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature,
                                const char* tblClass) {
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
  NumericVector xVec(nCoordinates);
  NumericVector yVec(nCoordinates);
  write_simple_geometry(context, geometry, xVec, yVec, 0);

  return new_geo_tbl(xVec, yVec, feature, tblClass);
}

List simple_geometry_to_xy(GEOSContextHandle_t context, GEOSGeometry* geometry) {
  const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(context, geometry);
  unsigned int size;
  GEOSCoordSeq_getSize_r(context, seq, &size);

  NumericVector xVec(size);
  NumericVector yVec(size);
  double x;
  double y;

  for (int i=0; i<size; i++) {
    GEOSCoordSeq_getX_r(context, seq, i, &x);
    GEOSCoordSeq_getY_r(context, seq, i, &y);
    xVec[i] = x;
    yVec[i] = y;
  }

  List xy = List::create(_["x"] = xVec, _["y"] = yVec);
  xy.attr("class") = CharacterVector::create("geo_xy", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
  return xy;
}

List new_geo_tbl(NumericVector x, NumericVector y, int feature, const char* tblClass) {
  List xy = List::create(_["x"] = x, _["y"] = y);
  xy.attr("class") = CharacterVector::create("geo_xy", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
  List output = List::create(
    _["xy"] = xy,
    _["feature"] = rep(feature, x.size())
  );

  output.attr("class") = CharacterVector::create(tblClass, "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
  return output;
}

List new_geo_tbl(NumericVector x, NumericVector y, IntegerVector piece, int feature, const char* tblClass) {
  List output = new_geo_tbl(x, y, feature, tblClass);
  output.push_back(piece, "piece");
  output.attr("class") = CharacterVector::create(tblClass, "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
  return output;
}

unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                   NumericVector xVec, NumericVector yVec, IntegerVector part,
                                   int partId, int offset) {
  unsigned int size = write_simple_geometry(context, geometry, xVec, yVec, offset);
  for(int i=0; i<size; i++) {
    part[offset + i] = partId;
  }
  return size;
}

unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                   NumericVector xVec, NumericVector yVec, int offset) {
  const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(context, geometry);
  unsigned int size;
  GEOSCoordSeq_getSize_r(context, seq, &size);

  double x;
  double y;

  for (int i=0; i<size; i++) {
    GEOSCoordSeq_getX_r(context, seq, i, &x);
    GEOSCoordSeq_getY_r(context, seq, i, &y);
    xVec[offset + i] = x;
    yVec[offset + i] = y;
  }

  return size;
}

