
#' Binary predicate operators
#'
#' [Binary predicate operators](https://en.wikipedia.org/wiki/DE-9IM#Spatial_predicates) take
#' two geometries and return `TRUE` or `FALSE` for each geometry pair.
#'
#' \describe{
#'   \item{[geos_disjoint()]}{
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
#'     of [geos_disjoint()].
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
geos_disjoint <- function(x, y) {
  cpp_binary_predicate(x, y, BP_DISJOINT)
}

#' @rdname geos_disjoint
#' @export
geos_touches <- function(x, y) {
  cpp_binary_predicate(x, y, BP_TOUCHES)
}

#' @rdname geos_disjoint
#' @export
geos_intersects <- function(x, y) {
  cpp_binary_predicate(x, y, BP_INTERSECTS)
}

#' @rdname geos_disjoint
#' @export
geos_crosses <- function(x, y) {
  cpp_binary_predicate(x, y, BP_CROSSES)
}

#' @rdname geos_disjoint
#' @export
geos_is_within <- function(x, y) {
  cpp_binary_predicate(x, y, BP_IS_WITHIN)
}

#' @rdname geos_disjoint
#' @export
geos_contains <- function(x, y) {
  cpp_binary_predicate(x, y, BP_CONTAINS)
}

#' @rdname geos_disjoint
#' @export
geos_overlaps <- function(x, y) {
  cpp_binary_predicate(x, y, BP_OVERLAPS)
}

#' @rdname geos_disjoint
#' @export
geos_equals <- function(x, y) {
  cpp_binary_predicate(x, y, BP_EQUALS)
}

#' @rdname geos_disjoint
#' @export
geos_covers <- function(x, y) {
  cpp_binary_predicate(x, y, BP_COVERS)
}

#' @rdname geos_disjoint
#' @export
geos_is_covered_by <- function(x, y) {
  cpp_binary_predicate(x, y, BP_IS_COVERED_BY)
}

BP_DISJOINT <- 1
BP_TOUCHES <- 2
BP_INTERSECTS <- 3
BP_CROSSES <- 4
BP_IS_WITHIN <- 5
BP_CONTAINS <- 6
BP_OVERLAPS <- 7
BP_EQUALS <- 8
BP_EQUALS_EXACT <- 9
BP_COVERS <- 10
BP_IS_COVERED_BY <- 11
