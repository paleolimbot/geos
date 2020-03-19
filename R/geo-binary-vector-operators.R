
#' Binary predicate operators
#'
#' [Binary predicate operators](https://en.wikipedia.org/wiki/DE-9IM#Spatial_predicates) take
#' two geometries and return `TRUE` or `FALSE` for each geometry pair.
#'
#' \describe{
#'   \item{[geo_disjoint()]}{
#'     Returns `TRUE` if `x` and `y` *do not* have a point in common. Is
#'     the opposite of [geo_intersects()].
#'   }
#'   \item{[geo_touches()]}{
#'     Returns `TRUE` if `x` and `y` have a point in common, but
#'     their interiors do not intersect.
#'   }
#'   \item{[geo_intersects()]}{
#'     Returns `TRUE` if `x` and `y` have a point in common, regardless
#'     of whether or not their interiors intersect. Is the opposite
#'     of [geo_disjoint()].
#'   }
#'   \item{[geo_within()]}{
#'     Returns `TRUE` if all points in `x` are are also within `y`.
#'     `geo_within(x, y)` is identical to `geo_contains(y, x)`.
#'   }
#'   \item{[geo_contains()]}{
#'     Returns `TRUE` if all points in `y` are are also within `x`.
#'     `geo_contains(x, y)` is identical to `geo_within(y, x)`.
#'   }
#'   \item{[geo_overlaps()]}{
#'     Returns `TRUE` if `x` and `y` have *some* but *not all* points
#'     in common, and the intersection of the two is the same geometry
#'     type as the input. For example, two polygons can overlap, but
#'     a polygon and a line cannot.
#'   }
#'   \item{[geo_crosses()]}{
#'     Returns `TRUE` if `x` and `y` have *some* but *not all* points
#'     in common, and the intersection of the two is the *not* the
#'     same geometry type as the input. For example, a polygon
#'     and a line can cross, but two polygons cannot.
#'   }
#'   \item{[geo_equals()]}{
#'     Returns `TRUE` if `x` and `y` have *all* points in common and no
#'     points that intersect the exterior of the other.
#'   }
#'   \item{[geo_covers()]}{
#'     Returns `TRUE` if every point in `y` is a point of `x`. Is the opposite
#'     of [geo_covered_by()].
#'   }
#'   \item{[geo_covered_by()]}{
#'     Returns `TRUE` if every point in `x` is a point of `y`. Is the opposite
#'     of [geo_covered_by()].
#'   }
#' }
#'
#' @inheritParams geo_intersection
#'
#' @return A logical vector
#' @export
#'
geo_disjoint <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_DISJOINT)
}

#' @rdname geo_disjoint
#' @export
geo_touches <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_TOUCHES)
}

#' @rdname geo_disjoint
#' @export
geo_intersects <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_INTERSECTS)
}

#' @rdname geo_disjoint
#' @export
geo_crosses <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_CROSSES)
}

#' @rdname geo_disjoint
#' @export
geo_within <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_WITHIN)
}

#' @rdname geo_disjoint
#' @export
geo_contains <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_CONTAINS)
}

#' @rdname geo_disjoint
#' @export
geo_overlaps <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_OVERLAPS)
}

#' @rdname geo_disjoint
#' @export
geo_equals <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_EQUALS)
}

#' @rdname geo_disjoint
#' @export
geo_covers <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_COVERS)
}

#' @rdname geo_disjoint
#' @export
geo_covered_by <- function(x, y) {
  geomcpp_binary_predicate(x, y, BP_COVERED_BY)
}

BP_DISJOINT <- 1
BP_TOUCHES <- 2
BP_INTERSECTS <- 3
BP_CROSSES <- 4
BP_WITHIN <- 5
BP_CONTAINS <- 6
BP_OVERLAPS <- 7
BP_EQUALS <- 8
BP_EQUALS_EXACT <- 9
BP_COVERS <- 10
BP_COVERED_BY <- 11
