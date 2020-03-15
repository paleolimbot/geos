
#ifndef GEOS_COORDS_H
#define GEOS_COORDS_H

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

List geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List simple_multi_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List simple_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List simple_geometry_to_xy(GEOSContextHandle_t context, GEOSGeometry* geometry);

#endif
