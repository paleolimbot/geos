
#' Distance calculations
#'
#' @param geom1,geom2 [GEOS geometry vectors][as_geos_geometry],
#'  recycled to a common length.
#' @param densify A fraction between 0 and 1 denoting the degree to which
#'  edges should be subdivided (smaller value means more subdivisions).
#'  Use NULL to calculate the distance as-is.
#'
#' @return A numeric vector along the recycled length of `geom1` and `geom2`
#' @export
#'
geos_distance <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_distance, recycled[[1]], recycled[[2]])
}

#' @rdname geos_distance
#' @export
geos_distance_indexed <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_distance_indexed, recycled[[1]], recycled[[2]])
}

#' @rdname geos_distance
#' @export
geos_distance_hausdorff <- function(geom1, geom2, densify = NULL) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))

  if (is.null(densify)) {
    .Call(geos_c_distance_hausdorff, recycled[[1]], recycled[[2]])
  } else {
    .Call(geos_c_distance_hausdorff_densify, recycled[[1]], recycled[[2]], as.numeric(densify))
  }
}

#' @rdname geos_distance
#' @export
geos_distance_frechet <- function(geom1, geom2, densify = NULL) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))

  if (is.null(densify)) {
    .Call(geos_c_distance_frechet, recycled[[1]], recycled[[2]])
  } else {
    .Call(geos_c_distance_frechet_densify, recycled[[1]], recycled[[2]], as.numeric(densify))
  }
}

#' Linear referencing
#'
#' - [geos_project()] and [geos_project_normalized()] return
#'   the distance of point `geom2` projected on `geom1` from the origin
#'   of `geom1`, which must be a lineal geometry.
#' - [geos_interpolate()] performs an
#'   inverse operation, returning the point along `geom` representing
#'   the given `distance` from the origin along the geometry.
#' - `_normalized()` variants use a distance normalized to the
#'   [geos_length()] of the geometry.
#'
#' @inheritParams geos_distance
#' @inheritParams geos_read_wkt
#' @param distance Distance along the linestring to interpolate
#' @param distance_normalized Distance along the linestring to interpolate
#'   relative to the length of the linestring.
#'
#' @export
#'
geos_project <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_project, recycled[[1]], recycled[[2]])
}

#' @rdname geos_project
#' @export
geos_project_normalized <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_project_normalized, recycled[[1]], recycled[[2]])
}

#' Binary predicates
#'
#' @inheritParams geos_distance
#' @param tolerance The maximum separation of vertices that should
#'   be considered equal.
#'
#' @return A logical vector along the recycled length of `geom1` and `geom2`
#' @export
#'
geos_disjoint <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_disjoint, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_touches <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_touches, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_intersects <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_intersects, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_crosses <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_crosses, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_within <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_within, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_contains <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_contains, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_overlaps <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_overlaps, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_equals <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_equals, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_equals_exact <- function(geom1, geom2, tolerance = .Machine$double.eps ^ 2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2), as.numeric(tolerance)))
  .Call(geos_c_equals_exact, recycled[[1]], recycled[[2]], recycled[[3]])
}

#' @rdname geos_disjoint
#' @export
geos_covers <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_covers, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_covered_by <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_covered_by, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_disjoint <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_disjoint, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_touches <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_touches, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_intersects <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_intersects, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_crosses <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_crosses, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_within <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_within, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_contains <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_contains, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_contains_properly <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_contains_properly, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_overlaps <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_overlaps, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_covers <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_covers, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_covered_by <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_covered_by, recycled[[1]], recycled[[2]])
}
