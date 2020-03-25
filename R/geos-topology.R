
#' Geometry summaries
#'
#' These functions return geometries that summarize the input in
#' various ways.
#'
#' @inheritParams geo_ptype
#'
#' @export
#'
#'
geos_centroid <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_centroid(x, to))
}

#' @rdname geos_centroid
#' @export
geos_point_on_surface <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_point_on_surface(x, to))
}

#' @rdname geos_centroid
#' @export
geos_nodes <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_nodes(x, to))
}

#' @rdname geos_centroid
#' @export
geos_boundary <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_boundary(x, to))
}

#' @rdname geos_centroid
#' @export
geos_envelope <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_envelope(x, to))
}

#' @rdname geos_centroid
#' @export
geos_convex_hull <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_convex_hull(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_rotated_rectangle <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_minimum_rotated_rectangle(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_bounding_circle <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_minimum_bounding_circle(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_width <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_minimum_width(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_clearance_line <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_minimum_clearance_line(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_clearance <- function(x) {
  cpp_minimum_clearance(x)
}


