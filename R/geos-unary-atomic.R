
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
