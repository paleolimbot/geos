
#ifndef GEOS_COORDS_H
#define GEOS_COORDS_H

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

List geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List polygon_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature);
List simple_multi_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry,
                                      int feature, const char* tblClass);
List simple_geometry_to_geo_tbl(GEOSContextHandle_t context, GEOSGeometry* geometry, int feature,
                                const char* tblClass);
List simple_geometry_to_xy(GEOSContextHandle_t context, GEOSGeometry* geometry);
List new_geo_tbl(NumericVector x, NumericVector y, int feature, const char* tblClass);
List new_geo_tbl(NumericVector x, NumericVector y, IntegerVector piece, int feature, const char* tblClass);
unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                   NumericVector xVec, NumericVector yVec, IntegerVector part,
                                   int partId, int offset);
unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                   NumericVector xVec, NumericVector yVec, int offset);

#endif
