
#' Create and validate well-known text
#'
#' @param x A character vector containing well-known text
#'
#' @return A [new_geo_wkt()]
#' @export
#'
#' @examples
#' geo_wkt("POINT (30 10)")
#'
geo_wkt <- function(x = character()) {
  x <- vec_cast(x, character())
  wkt <- validate_geo_wkt(new_geo_wkt(x))
  wkt
}


#' S3 details for geo_wkt
#'
#' @inheritParams geo_wkt
#' @param ... Unused
#'
#' @export
#'
#' @examples
#' wkt <- geo_wkt("POINT (30 10)")
#' is_geo_wkt(wkt)
#'
new_geo_wkt <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = c("geo_wkt", "geo"))
}

#' @rdname new_geo_wkt
#' @export
is_geo_wkt <- function(x) {
  inherits(x, "geo_wkt")
}

#' @rdname new_geo_wkt
#' @export
validate_geo_wkt <- function(x) {
  is_parseable <- geos_wkt_is_parseable(x)
  stop_for_non_parseable(is_parseable)
  invisible(x)
}

#' @rdname new_geo_wkt
#' @export
vec_ptype_abbr.geo_wkt <- function(x, ...) {
  "wkt"
}
