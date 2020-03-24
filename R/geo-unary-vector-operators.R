
#' Unary geometry predicates
#'
#' Unary geometry predicates take one geometry and  return a logical vector.
#'
#' \describe{
#'   \item{[geos_is_empty()]}{
#'     Returns `TRUE` if `x` contains no coordinates.
#'   }
#'   \item{[geos_is_simple()]}{
#'     Returns `TRUE` if `x` does not intersect itself.
#'   }
#'   \item{[geos_has_z()]}{
#'     Returns `TRUE` if `x` contains z coordinates.
#'   }
#'   \item{[geos_is_closed()]}{
#'     Returns `TRUE` if the first coordinate of `x` is also the
#'     last coordinate of `x`.
#'   }
#' }
#'
#' @inheritParams geo_ptype
#'
#' @return A logical vector
#' @export
#'
#' @examples
#' geos_is_empty(geo_wkt("POINT EMPTY"))
#' geos_is_empty(geo_wkt("POINT (30 10)"))
#'
#'
#'
geos_is_empty <- function(x) {
  cpp_unary_predicate(x, UP_IS_EMPTY)
}

#' @rdname geos_is_empty
#' @export
geos_is_simple <- function(x) {
  cpp_unary_predicate(x, UP_IS_SIMPLE)
}

#' @rdname geos_is_empty
#' @export
geos_has_z <- function(x) {
  cpp_unary_predicate(x, UP_HAS_Z)
}

#' @rdname geos_is_empty
#' @export
geos_is_closed <- function(x) {
  cpp_unary_predicate(x, UP_IS_CLOSED)
}

UP_IS_EMPTY <- 1
UP_IS_SIMPLE <- 2
UP_IS_RING <- 3
UP_HAS_Z <- 4
UP_IS_CLOSED <- 5
