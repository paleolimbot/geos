
#' Extract information from a GEOS geometry
#'
#' Note that [geos_x()], [geos_y()], and [geos_z()] do not handle
#' empty points (use [geos_write_xy()] if you need to handle this case).
#' Similarly, the min/max functions will error on empty geometries.
#'
#' @inheritParams geos_read_wkt
#'
#' @return A vector of length `geom`
#' @export
#'
#' @examples
#' geos_area("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
#' geos_length("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
#' geos_x("POINT Z (1 2 3)")
#' geos_y("POINT Z (1 2 3)")
#' geos_z("POINT Z (1 2 3)")
#' geos_xmin("LINESTRING (0 1, 2 3)")
#' geos_ymin("LINESTRING (0 1, 2 3)")
#' geos_xmax("LINESTRING (0 1, 2 3)")
#' geos_ymax("LINESTRING (0 1, 2 3)")
#' geos_minimum_clearance("POLYGON ((0 0, 10 0, 10 10, 3 5, 0 10, 0 0))")
#'
#' geos_is_empty(c("POINT EMPTY", "POINT (0 1)"))
#' geos_is_simple(c("LINESTRING (0 0, 1 1)", "LINESTRING (0 0, 1 1, 1 0, 0 1)"))
#' geos_is_ring(
#'   c(
#'     "LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)",
#'     "LINESTRING (0 0, 1 0, 1 1, 0 1)"
#'    )
#' )
#' geos_is_closed(
#'   c(
#'     "LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)",
#'     "LINESTRING (0 0, 1 0, 1 1, 0 1)"
#'    )
#' )
#' geos_is_valid(
#'   c(
#'     "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))",
#'     "POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))"
#'   )
#' )
#' geos_has_z(c("POINT Z (1 2 3)", "POINT (1 2)"))
#'
#' geos_type_id(c("POINT (0 0)", "LINESTRING (0 0, 1 1)"))
#' geos_srid(wk::as_wkb(c("SRID=1234;POINT (0 0)", "POINT (0 0)")))
#' geos_num_coordinates(c("POINT (0 0)", "MULTIPOINT (0 0, 1 1)"))
#' geos_num_geometries(c("POINT (0 0)", "MULTIPOINT (0 0, 1 1)"))
#' geos_num_interior_rings("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
#' geos_dimension(c("POINT (0 0)", "LINESTRING (0 0, 1 1)"))
#' geos_coordinate_dimension(c("POINT (0 0)", "POINT Z (0 0 1)"))
#'
geos_area <- function(geom) {
  .Call(geos_c_area, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_length <- function(geom) {
  .Call(geos_c_length, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_x <- function(geom) {
  .Call(geos_c_x, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_y <- function(geom) {
  .Call(geos_c_y, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_z <- function(geom) {
  .Call(geos_c_z, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_xmin <- function(geom) {
  .Call(geos_c_xmin, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_ymin <- function(geom) {
  .Call(geos_c_ymin, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_xmax <- function(geom) {
  .Call(geos_c_xmax, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_ymax <- function(geom) {
  .Call(geos_c_ymax, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_minimum_clearance <- function(geom) {
  .Call(geos_c_minimum_clearance, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_is_empty <- function(geom) {
  .Call(geos_c_is_empty, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_is_simple <- function(geom) {
  .Call(geos_c_is_simple, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_is_ring <- function(geom) {
  .Call(geos_c_is_ring, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_has_z <- function(geom) {
  .Call(geos_c_has_z, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_is_closed <- function(geom) {
  .Call(geos_c_is_closed, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_is_valid <- function(geom) {
  .Call(geos_c_is_valid, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_type_id <- function(geom) {
  # in a slight departure from GEOS, returning the WKB
  # type IDs to avoid confusion (the problem is the LINEARRING)
  match(.Call(geos_c_type_id, as_geos_geometry(geom)), c(0:1, 3:8))
}

#' @rdname geos_area
#' @export
geos_type <- function(geom) {
  c(
    "point", "linestring", "linearring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )[.Call(geos_c_type_id, as_geos_geometry(geom)) + 1]
}

#' @rdname geos_area
#' @export
geos_precision <- function(geom) {
  .Call(geos_c_precision, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_srid <- function(geom) {
  .Call(geos_c_srid, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_num_coordinates <- function(geom) {
  .Call(geos_c_num_coordinates, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_num_geometries <- function(geom) {
  .Call(geos_c_num_geometries, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_num_interior_rings <- function(geom) {
  .Call(geos_c_num_interior_rings, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_dimension <- function(geom) {
  .Call(geos_c_dimension, as_geos_geometry(geom))
}

#' @rdname geos_area
#' @export
geos_coordinate_dimension <- function(geom) {
  .Call(geos_c_coorinate_dimension, as_geos_geometry(geom))
}
