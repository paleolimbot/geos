
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
  geomcpp_is_empty(x)
}
