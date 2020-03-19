
#' Check for empty geometries
#'
#' @inheritParams geo_ptype
#'
#' @return A logical vector
#' @export
#'
#' @examples
#' geo_is_empty(geo_wkt("POINT EMPTY"))
#' geo_is_empty(geo_wkt("POINT (30 10)"))
#'
geo_is_empty <- function(x) {
  geomcpp_unary_predicate(x, UP_IS_EMPTY)
}

#' @rdname geo_is_empty
#' @export
geo_is_simple <- function(x) {
  geomcpp_unary_predicate(x, UP_IS_SIMPLE)
}

#' @rdname geo_is_empty
#' @export
geo_has_z <- function(x) {
  geomcpp_unary_predicate(x, UP_HAS_Z)
}

#' @rdname geo_is_empty
#' @export
geo_is_closed <- function(x) {
  geomcpp_unary_predicate(x, UP_IS_CLOSED)
}

UP_IS_EMPTY <- 1
UP_IS_SIMPLE <- 2
UP_IS_RING <- 3
UP_HAS_Z <- 4
UP_IS_CLOSED <- 5
