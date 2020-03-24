
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
  cpp_is_empty(x)
}

#' @rdname geos_is_empty
#' @export
geos_is_simple <- function(x) {
  cpp_is_simple(x)
}

#' @rdname geos_is_empty
#' @export
geos_has_z <- function(x) {
  cpp_has_z(x)
}

#' @rdname geos_is_empty
#' @export
geos_is_closed <- function(x) {
  cpp_is_closed(x)
}
