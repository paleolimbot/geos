
#' Segment operations
#'
#' @param x,y [geo_segment()]s
#' @param point A [geo_xy()]
#'
#' @return [geos_segment_intersection()] returns a [geo_xy()];
#'   [geos_orientation_index()] returns -1, 0 or 1, depending if
#'   the `point` lies to the right of (-1), is colinear with (0) or
#'   lies to the left of (1) the segment (as judged from the start
#'   of the segment looking towards the end).
#' @export
#'
#' @examples
#' geos_segment_intersection(
#'   geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
#'   geo_segment(geo_xy(10, 0), geo_xy(0, 10))
#' )
#'
#' geos_orientation_index(
#'   geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
#'   geo_xy(15, c(12, 15, 17))
#' )
#'
geos_segment_intersection <- function(x, y) {
  cpp_segment_intersection(
    field(field(x, "start"), "x"), field(field(x, "start"), "y"),
    field(field(x, "end"), "x"), field(field(x, "end"), "y"),
    field(field(y, "start"), "x"), field(field(y, "start"), "y"),
    field(field(y, "end"), "x"), field(field(y, "end"), "y")
  )
}

#' @rdname geos_segment_intersection
#' @export
geos_orientation_index <- function(x, point) {
  recycled <- vec_recycle_common(x = x, point = point)

  cpp_orientation_index(
    field(field(recycled$x, "start"), "x"), field(field(recycled$x, "start"), "y"),
    field(field(recycled$x, "end"), "x"), field(field(recycled$x, "end"), "y"),
    field(recycled$point, "x"), field(recycled$point, "y")
  )
}
