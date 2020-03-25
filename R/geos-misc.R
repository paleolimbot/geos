
#' Area, length, and distance
#'
#' @inheritParams geos_intersection
#'
#' @return
#' - [geos_area()] computes areas for polygons, or returns 0 otherwise.
#' - [geos_length()] computes the length of the boundary for polygons, or the length
#'   of the line for linestrings.
#' - [geos_distance()] returns the smallest possible distance between the two
#'   geometries.
#'
#' @export
#'
#' @examples
#' geos_area(geo_wkt("POLYGON ((0 0, 10 0, 0 10, 0 0))"))
#' geos_length(geo_wkt("POLYGON ((0 0, 10 0, 0 10, 0 0))"))
#' geos_distance(
#'   geo_wkt("POLYGON ((0 0, 10 0, 0 10, 0 0))"),
#'   geo_wkt("POINT (10 10)")
#' )
#'
geos_area <- function(x) {
  cpp_area(x)
}

#' @rdname geos_area
#' @export
geos_length <- function(x) {
  cpp_length(x)
}

#' @rdname geos_area
#' @export
geos_distance <- function(x, y) {
  cpp_distance(x, y)
}
