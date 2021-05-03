
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
#' @param prepare Use prepared geometries to calculate clearance line
#' @param grid_size For `_prec()` variants, the grid size such that all vertices of
#'   the resulting geometry will lie on the grid.
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
  recycled <- recycle_common(list(sanitize_geos_geometry(geom1), sanitize_geos_geometry(geom2)))
  new_geos_geometry(
    .Call(geos_c_intersection, recycled[[1]], recycled[[2]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_difference <- function(geom1, geom2) {
  recycled <- recycle_common(list(sanitize_geos_geometry(geom1), sanitize_geos_geometry(geom2)))
  new_geos_geometry(
    .Call(geos_c_difference, recycled[[1]], recycled[[2]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_sym_difference <- function(geom1, geom2) {
  recycled <- recycle_common(list(sanitize_geos_geometry(geom1), sanitize_geos_geometry(geom2)))
  new_geos_geometry(
    .Call(geos_c_sym_difference, recycled[[1]], recycled[[2]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_union <- function(geom1, geom2) {
  recycled <- recycle_common(list(sanitize_geos_geometry(geom1), sanitize_geos_geometry(geom2)))
  new_geos_geometry(
    .Call(geos_c_union, recycled[[1]], recycled[[2]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_intersection_prec <- function(geom1, geom2, grid_size) {
  recycled <- recycle_common(
    list(
      sanitize_geos_geometry(geom1),
      sanitize_geos_geometry(geom2),
      sanitize_double(grid_size)
    )
  )
  new_geos_geometry(
    .Call(geos_c_intersection_prec, recycled[[1]], recycled[[2]], recycled[[3]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_difference_prec <- function(geom1, geom2, grid_size) {
  recycled <- recycle_common(
    list(
      sanitize_geos_geometry(geom1),
      sanitize_geos_geometry(geom2),
      sanitize_double(grid_size)
    )
  )
  new_geos_geometry(
    .Call(geos_c_difference_prec, recycled[[1]], recycled[[2]], recycled[[3]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_sym_difference_prec <- function(geom1, geom2, grid_size) {
  recycled <- recycle_common(
    list(
      sanitize_geos_geometry(geom1),
      sanitize_geos_geometry(geom2),
      sanitize_double(grid_size)
    )
  )
  new_geos_geometry(
    .Call(geos_c_sym_difference_prec, recycled[[1]], recycled[[2]], recycled[[3]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_union_prec <- function(geom1, geom2, grid_size) {
  recycled <- recycle_common(
    list(
      sanitize_geos_geometry(geom1),
      sanitize_geos_geometry(geom2),
      sanitize_double(grid_size)
    )
  )
  new_geos_geometry(
    .Call(geos_c_union_prec, recycled[[1]], recycled[[2]], recycled[[3]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_shared_paths <- function(geom1, geom2) {
  recycled <- recycle_common(list(sanitize_geos_geometry(geom1), sanitize_geos_geometry(geom2)))
  new_geos_geometry(
    .Call(geos_c_shared_paths, recycled[[1]], recycled[[2]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_snap <- function(geom1, geom2, tolerance = .Machine$double.eps ^ 2) {
  recycled <- recycle_common(
    list(
      sanitize_geos_geometry(geom1),
      sanitize_geos_geometry(geom2),
      sanitize_double(tolerance)
    )
  )
  new_geos_geometry(
    .Call(geos_c_snap, recycled[[1]], recycled[[2]], recycled[[3]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_intersection
#' @export
geos_clearance_line_between <- function(geom1, geom2, prepare = FALSE) {
  recycled <- recycle_common(
    list(
      sanitize_geos_geometry(geom1),
      sanitize_geos_geometry(geom2)
    )
  )

  prepare <- sanitize_logical_scalar(prepare)

  new_geos_geometry(
    .Call(geos_c_clearance_line_between, recycled[[1]], recycled[[2]], prepare),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

# documented with other circle functions in geos-unary-geometry.R
#' @rdname geos_minimum_bounding_circle
#' @export
geos_largest_empty_circle_spec <- function(geom, boundary, tolerance) {
  recycled <- recycle_common(
    list(
      sanitize_geos_geometry(geom),
      sanitize_geos_geometry(boundary),
      sanitize_double(tolerance)
    )
  )

  new_geos_geometry(
    .Call(geos_c_largest_empty_circle, recycled[[1]], recycled[[2]], recycled[[3]]),
    crs = wk_crs_output(recycled[[1]], recycled[[2]])
  )
}

#' @rdname geos_minimum_bounding_circle
#' @export
geos_largest_empty_crc <- function(geom, boundary, tolerance) {
  spec <- geos_largest_empty_circle_spec(geom, boundary, tolerance)
  xy <- unclass(as_xy(geos_point_end(spec)))

  wk::crc(
    xy$x, xy$y,
    geos_length(spec),
    crs = attr(spec, "crs", exact = TRUE)
  )
}
