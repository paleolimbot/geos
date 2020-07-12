
#' Distance calculations
#'
#' @param geom1,geom2 [GEOS geometry vectors][as_geos_geometry],
#'  recycled to a common length.
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
geos_distance_hausdorff <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_distance_hausdorff, recycled[[1]], recycled[[2]])
}

#' @rdname geos_distance
#' @export
geos_distance_frechet <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_distance_frechet, recycled[[1]], recycled[[2]])
}


#' Binary predicates
#'
#' @inheritParams geos_distance
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
