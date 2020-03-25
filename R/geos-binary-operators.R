
#' Intersect two geometry vectors
#'
#' @inheritParams geo_ptype
#' @param y A geometry-like object. `x` and `y` of length 1
#'   is recycled to the length of the other (like [tibble::tibble()]).
#' @param rect A [geo_rect()] used for the non-robust clipping
#'   algorithm.
#'
#' @return A geometry-like object, in the format defined by `to`.
#' @export
#'
#' @examples
#' poly1 <- geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
#' poly2 <- geo_wkt("POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))")
#'
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geos_intersection(poly1, poly2), col = "grey90")
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
