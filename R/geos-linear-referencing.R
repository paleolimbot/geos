
#' Linear referencing
#'
#' Vectorized along `x` and `distance`.
#'
#' @inheritParams geo_ptype
#' @param x A geometry-like (linestring) along which to interpolate
#'   (or on which to project).
#' @param point A (point) geometry-like object
#' @param distance The distance along the geometry.
#'
#' @return [geos_project()] and [geos_project_normalized()] both return
#'   a geometry-like point vector; [geos_interpolate()] and
#'   [geos_interpolate_normalized()] both return
#'
#' @export
#'
#' @examples
#' line <- geo_wkt("LINESTRING (0 0, 10 10)")
#' geo_plot(line)
#'
#' # geos_interpolate() works in absolute distances
#' geo_plot_add(
#'   geos_interpolate(line, c(1, 5, 10)),
#'   col = "red"
#' )
#'
#' # geos_interpolate() works in relative distances (from 0..1)
#' # this is useful for generating a number of equally-spaced points
#' # along a line
#' geo_plot_add(
#'    geos_interpolate_normalized(line, seq(0, 1, length.out = 5)),
#'   col = "blue"
#' )
#'
#' # geos_project() and geos_project_normalized() are the opposite
#' # of these operations
#' geos_project(line, geo_wkt("POINT (5 5)"))
#' geos_project_normalized(line, geo_wkt("POINT (5 5)"))
#'
geos_project <- function(x, point) {
  cpp_project(x, point)
}

#' @rdname geos_project
#' @export
geos_project_normalized <- function(x, point) {
  cpp_project_normalized(x, point)
}

#' @rdname geos_project
#' @export
geos_interpolate <- function(x, distance, to = geo_ptype(x)) {
  distance <- rep_len_or_fail(distance, geo_size(x))
  geo_restore(cpp_interpolate(x, to, distance), to = to)
}

#' @rdname geos_project
#' @export
geos_interpolate_normalized <- function(x, distance, to = geo_ptype(x)) {
  distance <- rep_len_or_fail(distance, geo_size(x))
  geo_restore(cpp_interpolate_normalized(x, to, distance), to = to)
}
