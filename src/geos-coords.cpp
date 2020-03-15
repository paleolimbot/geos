
#include "geos-coords.h"
#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

List geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int type = GEOSGeomTypeId_r(context, geometry);
  List geoTbl;

  if (type == GEOSGeomTypes::GEOS_POINT) {
    CharacterVector cls = CharacterVector::create("geo_tbl_point", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
    geoTbl = simple_geometry_to_geo_tbl(context, geometry, feature);
    geoTbl.attr("class") = cls;

  } else if(type == GEOSGeomTypes::GEOS_LINESTRING) {
    CharacterVector cls = CharacterVector::create("geo_tbl_linestring", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
    geoTbl = simple_geometry_to_geo_tbl(context, geometry, feature);
    geoTbl.attr("class") = cls;

  } else if(type == GEOSGeomTypes::GEOS_POLYGON) {
    stop("Not implemented polygon");

  } else if(type == GEOSGeomTypes::GEOS_MULTIPOINT) {
    CharacterVector cls = CharacterVector::create("geo_tbl_multipoint", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
    geoTbl = simple_multi_geometry_to_geo_tbl(context, geometry, feature);
    geoTbl.attr("class") = cls;

  } else if(type == GEOSGeomTypes::GEOS_MULTILINESTRING) {
    CharacterVector cls = CharacterVector::create("geo_tbl_multilinestring", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");
    geoTbl = simple_multi_geometry_to_geo_tbl(context, geometry, feature);
    geoTbl.attr("class") = cls;

  } else if(type == GEOSGeomTypes::GEOS_MULTIPOLYGON) {
    stop("Not implemented multipolygon");

  } else {
    stop("Can only convert point, linestring, polygon, and multi- variants to a geo_tbl");
  }

  return geoTbl;
}

List simple_multi_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int nParts = GEOSGetNumGeometries_r(context, geometry);

  int nCoordinates[nParts];
  int totalCoordinates = 0;
  List xyParts(nParts);

  for(int i=0; i<nParts; i++) {
    const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
    List xy = simple_geometry_to_xy(context, (GEOSGeometry*) part);
    nCoordinates[i] = ((NumericVector) xy[0]).size();
    totalCoordinates += nCoordinates[i];
    xyParts[i] = xy;
  }

  // this is something like vec_c() for a list of xys
  NumericVector xAll(totalCoordinates);
  NumericVector yAll(totalCoordinates);
  IntegerVector partAll(totalCoordinates);

  int jStart = 0;
  int k ;
  for(int i=0; i<xyParts.size(); i++) {
    List xy = xyParts[i];
    NumericVector x = (NumericVector) xy[0];
    NumericVector y = (NumericVector) xy[1];

    for(int j=0; j<x.size(); j++) {
      k = jStart + j;
      xAll[k] = x[j];
      yAll[k] = y[j];
      partAll[k] = i + 1;
    }

    jStart += x.size();
  }

  List xyAll = List::create(_["x"] = xAll, _["y"] = yAll);
  xyAll.attr("class") = CharacterVector::create("geo_xy", "geo_tbl", "vctrs_rcrd", "vctrs_vctr");

  return List::create(
    _["xy"] = xyAll,
    _["feature"] = rep(feature, totalCoordinates),
    _["part"] = partAll
  );
}

List simple_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  List xy = simple_geometry_to_xy(context, geometry);
  int nrows = ((NumericVector) xy[0]).size();

  return List::create(
    _["xy"] = xy,
    _["feature"] = rep(feature, nrows)
  );
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
