
#' Binary predicate operators
#'
#' [Binary predicate operators](https://en.wikipedia.org/wiki/DE-9IM#Spatial_predicates) take
#' two geometries and return `TRUE` or `FALSE` for each geometry pair.
#'
#' \describe{
#'   \item{[geos_is_disjoint()]}{
#'     Returns `TRUE` if `x` and `y` *do not* have a point in common. Is
#'     the opposite of [geos_intersects()].
#'   }
#'   \item{[geos_touches()]}{
#'     Returns `TRUE` if `x` and `y` have a point in common, but
#'     their interiors do not intersect.
#'   }
#'   \item{[geos_intersects()]}{
#'     Returns `TRUE` if `x` and `y` have a point in common, regardless
#'     of whether or not their interiors intersect. Is the opposite
#'     of [geos_is_disjoint()].
#'   }
#'   \item{[geos_is_within()]}{
#'     Returns `TRUE` if all points in `x` are are also within `y`.
#'     `geos_within(x, y)` is identical to `geos_contains(y, x)`.
#'   }
#'   \item{[geos_contains()]}{
#'     Returns `TRUE` if all points in `y` are are also within `x`.
#'     `geos_contains(x, y)` is identical to `geos_within(y, x)`.
#'   }
#'   \item{[geos_overlaps()]}{
#'     Returns `TRUE` if `x` and `y` have *some* but *not all* points
#'     in common, and the intersection of the two is the same geometry
#'     type as the input. For example, two polygons can overlap, but
#'     a polygon and a line cannot.
#'   }
#'   \item{[geos_crosses()]}{
#'     Returns `TRUE` if `x` and `y` have *some* but *not all* points
#'     in common, and the intersection of the two is the *not* the
#'     same geometry type as the input. For example, a polygon
#'     and a line can cross, but two polygons cannot.
#'   }
#'   \item{[geos_equals()]}{
#'     Returns `TRUE` if `x` and `y` have *all* points in common and no
#'     points that intersect the exterior of the other.
#'   }
#'   \item{[geos_covers()]}{
#'     Returns `TRUE` if every point in `y` is a point of `x`. Is the opposite
#'     of [geos_is_covered_by()].
#'   }
#'   \item{[geos_is_covered_by()]}{
#'     Returns `TRUE` if every point in `x` is a point of `y`. Is the opposite
#'     of [geos_covers()].
#'   }
#' }
#'
#' @inheritParams geos_intersection
#'
#' @return A logical vector
#' @export
#'
geos_is_disjoint <- function(x, y) {
  cpp_is_disjoint(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_touches <- function(x, y) {
  cpp_touches(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_intersects <- function(x, y) {
  cpp_intersects(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_crosses <- function(x, y) {
  cpp_crosses(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_is_within <- function(x, y) {
  cpp_is_within(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_contains <- function(x, y) {
  cpp_contains(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_overlaps <- function(x, y) {
  cpp_overlaps(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_equals <- function(x, y) {
  cpp_equals(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_covers <- function(x, y) {
  cpp_covers(x, y)
}

#' @rdname geos_is_disjoint
#' @export
geos_is_covered_by <- function(x, y) {
  cpp_is_covered_by(x, y)
}
