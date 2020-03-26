
#' Geometry information
#'
#' - [geos_is_empty()] returns `TRUE` if `x` contains no coordinates.
#' - [geos_is_simple()] returns `TRUE` if `x` does not intersect itself.
#' - [geos_has_z()] returns `TRUE` if `x` contains z coordinates.
#' - [geos_is_closed()] returns `TRUE` if the first coordinate of `x` is also the
#'   last coordinate of `x`.
#' - [geos_geom_type_id()] returns a number identifying the geometry type
#'   (0-7, "point", "linestring", "linearring", "polygon",
#'   "multipoint", "multilinestring", "multipolygon",
#'   "geometrycollection"). This is more useful in the C++ API so that
#'   you can `switch()` on  the geometry type.
#' - [geos_geom_type()] returns the geometry type as a character vector.
#' - [geos_get_srid()] returns the spatial reference ID included in the
#'   geometry (can be embedded in EWKB). Often there is no embedded SRID,
#'   in which case 0 is returned.
#' - [geos_n_geometries()] returns the number of geometries for multi
#'   or collection types, or 1 otherwise.
#' - [geos_n_points()] returns the number of points in a linestring geometry
#' - [geos_n_interior_rings()] counts the interior rings in a polygon geometry
#' - [geos_n_dimensions()] refers to the dimensionality of the geometry,
#'   in the sense that points are 0d, lines are 1d, and polygons are 2d.
#'   Collection types are processed recursively, returning the maximum
#'   dimensionality of their contents.
#'
#' @inheritParams geo_ptype
#'
#' @export
#'
#' @examples
#' geos_is_empty(geo_wkt("POINT EMPTY"))
#' geos_is_empty(geo_wkt("POINT (30 10)"))
#'
#' # first geometry intersects itself
#' geos_is_simple(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0, 10 10)"))
#' geos_is_simple(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0)"))
#'
#' # first geometry is 3D
#' geos_has_z(geo_wkt("POINT (10 10 1)"))
#' geos_has_z(geo_wkt("POINT (10 10)"))
#'
#' # first geometry is closed
#' geos_is_closed(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0)"))
#' geos_is_closed(geo_wkt("LINESTRING (0 0, 0 10, 10 0)"))
#'
#' # counts geometries in mulit- and collection geometries
#' geos_n_geometries(geo_wkt("POINT EMPTY"))
#' geos_n_geometries(geo_wkt("POINT (0 0)"))
#' geos_n_geometries(geo_wkt("MULTIPOINT (0 0, 10 10)"))
#' geos_n_geometries(geo_wkt("GEOMETRYCOLLECTION(POINT (10 10))"))
#'
#' # counts coordinates and dimensions (useful for allocating memory)
#' geos_n_coordinates(geo_wkt("POINT EMPTY"))
#' geos_n_coordinates(geo_wkt("POINT (0 0)"))
#' geos_n_coordinates(geo_wkt("MULTIPOINT (0 0, 10 10)"))
#'
#' geos_n_coordinate_dimensions(geo_wkt("POINT (10 10)"))
#' geos_n_coordinate_dimensions(geo_wkt("POINT (10 10 0)"))
#'
#' # count nodes in a linestring
#' geos_n_points(geo_wkt("LINESTRING EMPTY"))
#' geos_n_points(geo_wkt("LINESTRING (0 0, 10 10)"))
#' geos_n_points(geo_wkt("LINESTRING (0 0, 5 5, 10 10)"))
#'
#' # get the number of interior rings
#' poly_hole <- geo_wkt("POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))")
#' poly <- geo_wkt("POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10))")
#' geos_n_interior_rings(geo_wkt("POLYGON EMPTY"))
#' geos_n_interior_rings(poly)
#' geos_n_interior_rings(poly_hole)
#'
#' # get the dimensionality of the geometry
#' # in the sense that points  are 0d, lines are 1d,
#' # and polygons are 2d
#' geos_n_dimensions(geo_wkt("POINT (10 10)"))
#' geos_n_dimensions(geo_wkt("LINESTRING (10 10, 0 0)"))
#' geos_n_dimensions(geo_wkt("POLYGON ((10 10, 0 0, 0 10, 10 10))"))
#' geos_n_dimensions(geo_wkt("GEOMETRYCOLLECTION(POINT (10 10))"))
#' geos_n_dimensions(geo_wkt("GEOMETRYCOLLECTION(LINESTRING (10 10, 0 0))"))
#'
geos_is_empty <- function(x) {
  cpp_is_empty(x)
}

#' @rdname geos_is_empty
#' @export
geos_is_simple <- function(x) {
  cpp_is_simple(x)
}

#' @rdname geos_is_empty
#' @export
geos_has_z <- function(x) {
  cpp_has_z(x)
}

#' @rdname geos_is_empty
#' @export
geos_is_closed <- function(x) {
  cpp_is_closed(x)
}

#' @rdname geos_is_empty
#' @export
geos_geom_type_id <- function(x) {
  cpp_geom_type_id(x)
}

#' @rdname geos_is_empty
#' @export
geos_geom_type <- function(x) {
  c(
    "point", "linestring", "linearring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )[cpp_geom_type_id(x) + 1]
}

#' @rdname geos_is_empty
#' @export
geos_get_srid <- function(x) {
  cpp_get_srid(x)
}

#' @rdname geos_is_empty
#' @export
geos_n_geometries <- function(x) {
  cpp_n_geometries(x)
}

#' @rdname geos_is_empty
#' @export
geos_n_coordinates <- function(x) {
  cpp_n_coordinates(x)
}

#' @rdname geos_is_empty
#' @export
geos_n_points <- function(x) {
  cpp_n_points(x)
}

#' @rdname geos_is_empty
#' @export
geos_n_interior_rings <- function(x) {
  cpp_n_interior_rings(x)
}

#' @rdname geos_is_empty
#' @export
geos_n_dimensions <- function(x) {
  cpp_n_dimensions(x)
}

#' @rdname geos_is_empty
#' @export
geos_n_coordinate_dimensions <- function(x) {
  cpp_n_coordinate_dimensions(x)
}
