
#include "geos-coords.h"
#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

// ----- this is needed for some vector functions ------

List geo_coord_reclass(List input, const char* tblClass) {
  input.attr("class") = CharacterVector::create(tblClass, "geo_coord", "vctrs_rcrd", "vctrs_vctr");
  return input;
}

// ------  unexported details  ----------

List new_geo_coord(NumericVector x, NumericVector y, int feature) {
  List xy = List::create(_["x"] = x, _["y"] = y);

  List output = List::create(
    _["xy"] = geo_coord_reclass(xy, "geo_xy"),
    _["feature"] = rep(feature, x.size())
  );

  return output;
}

List new_geo_coord(NumericVector x, NumericVector y, IntegerVector piece, int feature) {
  List output = new_geo_coord(x, y, feature);
  output.push_back(piece, "piece");
  return output;
}

List new_geo_coord(NumericVector x, NumericVector y, IntegerVector part, IntegerVector piece, int feature) {
  List output = new_geo_coord(x, y, feature);
  output.push_back(part, "part");
  output.push_back(piece, "piece");
  return output;
}

unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                   NumericVector xVec, NumericVector yVec, int offset) {
  const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(context, geometry);
  unsigned int size;
  GEOSCoordSeq_getSize_r(context, seq, &size);

  double x;
  double y;

  for (unsigned int i=0; i<size; i++) {
    GEOSCoordSeq_getX_r(context, seq, i, &x);
    GEOSCoordSeq_getY_r(context, seq, i, &y);
    xVec[offset + i] = x;
    yVec[offset + i] = y;
  }

  return size;
}

unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                   NumericVector xVec, NumericVector yVec, IntegerVector part,
                                   int partId, int offset) {
  unsigned int size = write_simple_geometry(context, geometry, xVec, yVec, offset);
  for(unsigned int i=0; i<size; i++) {
    part[offset + i] = partId;
  }
  return size;
}

unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                   NumericVector xVec, NumericVector yVec, IntegerVector part,
                                   int partId, IntegerVector piece, int pieceId, int offset) {
  unsigned int size = write_simple_geometry(context, geometry, xVec, yVec, offset);
  for(unsigned int i=0; i<size; i++) {
    part[offset + i] = partId;
    piece[offset + i] = pieceId;
  }
  return size;
}

// ---------- higher level implementations ----------

List geometry_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int type = GEOSGeomTypeId_r(context, geometry);

  if (type == GEOSGeomTypes::GEOS_POINT) {
    return point_to_geo_coord(context, geometry, feature);

  } else if(type == GEOSGeomTypes::GEOS_LINESTRING) {
    return linestring_to_geo_coord(context, geometry, feature);

  } else if(type == GEOSGeomTypes::GEOS_POLYGON) {
    return polygon_to_geo_coord(context, geometry, feature);

  } else if(type == GEOSGeomTypes::GEOS_MULTIPOINT) {
    return multipoint_to_geo_coord(context, geometry, feature);

  } else if(type == GEOSGeomTypes::GEOS_MULTILINESTRING) {
    return multilinestring_to_geo_coord(context, geometry, feature);

  } else if(type == GEOSGeomTypes::GEOS_MULTIPOLYGON) {
    return multipolygon_to_geo_coord(context, geometry, feature);

  } else if(type == GEOSGeomTypes::GEOS_GEOMETRYCOLLECTION) {
    return geometrycollection_to_tbl(context, geometry, feature);

  } else {
    stop("Can only convert point, linestring, polygon, and multi- variants to a geo_coord");
  }
}

List point_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
  NumericVector xVec(nCoordinates);
  NumericVector yVec(nCoordinates);
  write_simple_geometry(context, geometry, xVec, yVec, 0);

  return geo_coord_reclass(new_geo_coord(xVec, yVec, feature), "geo_coord_point");
}

List linestring_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
  NumericVector xVec(nCoordinates);
  NumericVector yVec(nCoordinates);
  write_simple_geometry(context, geometry, xVec, yVec, 0);

  return geo_coord_reclass(new_geo_coord(xVec, yVec, feature), "geo_coord_linestring");
}

List polygon_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
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

  return geo_coord_reclass(new_geo_coord(xVec, yVec, piece, feature), "geo_coord_polygon");
}

List multipoint_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int nParts = GEOSGetNumGeometries_r(context, geometry);
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

  NumericVector xVec(nCoordinates);
  NumericVector yVec(nCoordinates);

  unsigned int offset = 0;

  for(int i=0; i<nParts; i++) {
    const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
    offset += write_simple_geometry(context, part, xVec, yVec, offset);
  }

  return geo_coord_reclass(new_geo_coord(xVec, yVec, feature), "geo_coord_multipoint");
}

List multilinestring_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry,
                                      int feature) {
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

  List output = new_geo_coord(xVec, yVec, feature);
  output.push_back(partVec, "part");
  return geo_coord_reclass(output, "geo_coord_multilinestring");
}

List multipolygon_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  int nParts = GEOSGetNumGeometries_r(context, geometry);
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

  NumericVector xVec(nCoordinates);
  NumericVector yVec(nCoordinates);
  IntegerVector partVec(nCoordinates);
  IntegerVector piece(nCoordinates);

  unsigned int offset = 0;

  for(int i=0; i<nParts; i++) {
    const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
    int nInteriorRings = GEOSGetNumInteriorRings_r(context, part);

    const GEOSGeometry* exterior = GEOSGetExteriorRing_r(context, part);
    offset += write_simple_geometry(
      context, exterior,
      xVec, yVec,
      partVec, i + 1,
      piece, 1,
      offset
    );

    for(int j=0; j<nInteriorRings; j++) {
      const GEOSGeometry* ring = GEOSGetInteriorRingN_r(context, part, j);
      offset += write_simple_geometry(
        context, ring,
        xVec, yVec,
        partVec, i + 1,
        piece, 2 + j,
        offset
      );
    }
  }

  return geo_coord_reclass(new_geo_coord(xVec, yVec, partVec, piece, feature), "geo_coord_multipolygon");
}

List geometrycollection_to_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) {
  // an empty collection is often returned in binary operations, so it is useful to handle this
  // case
  int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
  if (nCoordinates == 0) {
    return geo_coord_reclass(
      new_geo_coord(NumericVector::create(), NumericVector::create(), feature),
      "geo_coord_point"
    );
  } else {
    stop("Can't create a geo_coord_geometrycollection()");
  }
}
