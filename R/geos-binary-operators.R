
#' Main geometry operators
#'
#' - [geos_intersection()] returns the set of points common to both `x`
#'   and `y`.
#' - [geos_difference()] returns the set of points from `x` that are
#'   not contained by `y`.
#' - [geos_sym_difference()] returns the set of points that are *not*
#'   common to `x` and `y`.
#' - [geos_union()] returns the set of points contained by either `x`
#'   or `y`.
#' - [geos_unary_union()] works like [geos_union()], but only operates
#'   on one geometry (`x`). This is useful when `x` is a multi- or collection type
#'   that contains overlapping geometries.
#' - [geos_coverage_union()] is a fast union only for polygons that
#'   do not overlap.
#' - [geos_clip_by_rect()] is a fast intersection between a geometry
#'   and bounds defined by a [geo_rect()].
#'
#' @inheritParams geo_ptype
#' @param y A geometry-like object. `x` and `y` of length 1
#'   is recycled to the length of the other (like [tibble::tibble()]).
#' @param rect A [geo_rect()] used for the non-robust clipping
#'   algorithm.
#'
#' @return A geometry-like object, in the format defined by `to`.
#'   the geometry type is typically that of `x`, but may be
#'   promoted to a multi-type. These functions may return
#'   an empty geometry collection (e.g., for two geometries
#'   that do not intersect).
#' @export
#'
#' @examples
#' poly1 <- geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
#' poly2 <- geo_wkt("POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))")
#'
#' # intersection
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geos_intersection(poly1, poly2), col = "grey90")
#'
#' # difference
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geos_difference(poly1, poly2), col = "grey90")
#'
#' # symmetric difference
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geos_sym_difference(poly1, poly2), col = "grey90")
#'
#' # union
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geos_union(poly1, poly2), col = "grey90")
#'
#' # unary union is useful if you have multi- or collection
#' # geometries that may overlap
#' collection <- geo_wkt("
#'   GEOMETRYCOLLECTION (
#'     POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0)),
#'     POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))
#'   )
#' ")
#'
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geos_unary_union(collection), col = "grey90")
#'
#' # coverage union is only for polygons, and does not dissolve
#' # boundaries
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geos_coverage_union(collection), col = "grey90")
#'
#' # clip by rect performs a fast intersection with a rectangle
#' # similar to an intersection
#' geo_plot(poly1)
#' geo_plot_add(
#'   geos_clip_by_rect(poly1, geo_rect(-1, -1, 8, 8)),
#'   col = "grey90"
#' )
#'
geos_intersection <- function(x, y, to = geo_ptype(x)) {
  geo_restore(to, cpp_intersection(x, y, to))
}

#' @rdname geos_intersection
#' @export
geos_difference <- function(x, y, to = geo_ptype(x)) {
  geo_restore(to, cpp_difference(x, y, to))
}

#' @rdname geos_intersection
#' @export
geos_sym_difference <- function(x, y, to = geo_ptype(x)) {
  geo_restore(to, cpp_sym_difference(x, y, to))
}

#' @rdname geos_intersection
#' @export
geos_union <- function(x, y, to = geo_ptype(x)) {
  geo_restore(to, cpp_union(x, y, to))
}

#' @rdname geos_intersection
#' @export
geos_unary_union <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_unary_union(x, to))
}

#' @rdname geos_intersection
#' @export
geos_coverage_union <- function(x, to = geo_ptype(x)) {
  if (geos_version() < "3.8.0") {
    abort("Need GEOS >= 3.8.0 to use geos_coverage_union()")
  }

  geo_restore(to, cpp_coverage_union(x, to))
}

#' @rdname geos_intersection
#' @export
geos_clip_by_rect <- function(x, rect, to = geo_ptype(x)) {
  vec_assert(rect, geo_rect())
  rect <- rep_len_or_fail(rect, geo_size(x))

  geo_restore(
    to,
    cpp_clip_by_rect(
      x,
      xmin = field(rect, "xmin"),
      ymin = field(rect, "ymin"),
      xmax = field(rect, "xmax"),
      ymax = field(rect, "ymax"),
      to = to
    )
  )
}
