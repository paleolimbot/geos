
#' Binary geometry operators
#'
#' - [geos_intersection()] returns the set of points common to both `x`
#'   and `y`.
#' - [geos_difference()] returns the set of points from `x` that are
#'   not contained by `y`.
#' - [geos_sym_difference()] returns the set of points that are *not*
#'   common to `x` and `y`.
#' - [geos_union()] returns the set of points contained by either `x`
#'   or `y`.
#' - [geos_shared_paths()] returns a GEOMETRYCOLLECTION containing two
#'   MULTILINESTRINGS: the first containing paths in the same direction,
#'   the second containing common paths in the opposite direction.
#' - [geos_snap()] snaps the vertices of `x` within `tolerance` of `y`
#'   to `y`.
#'
#' @inheritParams geos_disjoint
#'
#' @return A [GEOS geometry vector][as_geos_geometry] along the recycled
#'   length of `geom1` and `geom2`.
#' @export
#'
#' @examples
#' poly1 <- "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
#' poly2 <- "POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))"
#'
#' geos_intersection(poly1, poly2)
#' geos_difference(poly1, poly2)
#' geos_sym_difference(poly1, poly2)
#' geos_union(poly1, poly2)
#'
#' line <- "LINESTRING (11 0, 11 10)"
#' geos_snap(poly1, line, tolerance = 2)
#'
#' geos_shared_paths("LINESTRING (0 0, 1 1, 2 2)", "LINESTRING (3 3, 2 2, 1 1)")
#'
geos_intersection <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  new_geos_geometry(.Call(geos_c_intersection, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_intersection
#' @export
geos_difference <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  new_geos_geometry(.Call(geos_c_difference, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_intersection
#' @export
geos_sym_difference <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  new_geos_geometry(.Call(geos_c_sym_difference, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_intersection
#' @export
geos_union <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  new_geos_geometry(.Call(geos_c_union, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_intersection
#' @export
geos_shared_paths <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  new_geos_geometry(.Call(geos_c_shared_paths, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_intersection
#' @export
geos_snap <- function(geom1, geom2, tolerance = .Machine$double.eps ^ 2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2), tolerance))
  new_geos_geometry(.Call(geos_c_snap, recycled[[1]], recycled[[2]], recycled[[3]]))
}
