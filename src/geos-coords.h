
#ifndef GEOS_COORDS_H
#define GEOS_COORDS_H

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

List geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);

List point_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List linestring_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List polygon_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);

List multipoint_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List multilinestring_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List multipolygon_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);

#endif
