
#ifndef GEOS_COORDS_H
#define GEOS_COORDS_H

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

List geo_coord_reclass(List input, const char* tblClass);

List geometry_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);

List point_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List linestring_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List polygon_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);

List multipoint_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List multilinestring_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List multipolygon_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List geometrycollection_to_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature) ;

#endif
