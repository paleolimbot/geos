
#' Extract information from a GEOS geometry
#'
#' @inheritParams geos_read_wkt
#'
#' @return A vector of length `geom`
#' @export
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
