
#' Segment operations
#'
#' @param a,b A `list()` representing segments in the form
#'   `list(x0, y0, x1, y1)`. List items with length 1 will be
#'    recycled to the length of the longest item.
#' @param point A `list()` representing points in the form `list(x, y)`.
#'
#' @return [geos_segment_intersection()] returns a `list(x, y)`;
#'   [geos_orientation_index()] returns -1, 0 or 1, depending if
#'   the `point` lies to the right of (-1), is colinear with (0) or
#'   lies to the left of (1) the segment (as judged from the start
#'   of the segment looking towards the end).
#' @export
#'
#' @examples
#' geos_segment_intersection(
#'   list(0, 0, 10, 10),
#'   list(10, 0, 0, 10)
#' )
#'
#' geos_orientation_index(
#'   list(0, 0, 10, 10),
#'   list(15, c(12, 15, 17))
#' )
#'
geos_segment_intersection <- function(a, b) {
  a <- geos_assert_list_of_numeric(a, 4, "a")
  b <- geos_assert_list_of_numeric(b, 4, "b")
  dots <- recycle_common(c(a, b))

  .Call(
    geos_c_segment_intersection,
    dots[[1]], dots[[2]], dots[[3]], dots[[4]],
    dots[[5]], dots[[6]], dots[[7]], dots[[8]]
  )
}

#' @rdname geos_segment_intersection
#' @export
geos_orientation_index <- function(a, point) {
  a <- geos_assert_list_of_numeric(a, 4, "a")
  point <- geos_assert_list_of_numeric(point, 2, "point")
  dots <- recycle_common(c(a, point))

  .Call(
    geos_c_orientation_index,
    dots[[1]], dots[[2]], dots[[3]], dots[[4]],
    dots[[5]], dots[[6]]
  )
}

geos_assert_list_of_numeric <- function(x, len, arg) {
  x <- unclass(x)
  if (!is.list(x) || (length(x) != len)) {
    stop(sprintf("`%s` must be a list() of 'numeric' with length %s", arg, len))
  }

  lapply(x, sanitize_double)
}
