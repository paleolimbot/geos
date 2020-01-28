
#' Create and validate well-known text
#'
#' @param x A character vector containing well-known text
#' @param ... Unused
#'
#' @return A vctr of class geo_wkt
#' @export
#'
#' @examples
#' geo_wkt("POINT (30 10)")
#'
geo_wkt <- function(x = character()) {
  x <- vec_cast(x, character())
  new_geo_wkt(x)
}

#' @rdname geo_wkt
#' @export
new_geo_wkt <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = c("geo_wkt", "geo"))
}

#' @rdname geo_wkt
#' @export
is_geo_wkt <- function(x) {
  inherits(x, "geo_wkt")
}

#' @rdname geo_wkt
#' @export
validate_geo_wkt <- function(x) {
  abort("Not implemented")
}

#' @rdname geo_wkt
#' @export
vec_ptype_abbr.vctrs_percent <- function(x, ...) {
  "wkt"
}
