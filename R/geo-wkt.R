
#' Create and validate well-known text
#'
#' Like other geo types, [geo_wkt()] doesn't convert its input
#' but does validate it using [validate_geo_wkt()].
#' To skip validation, use [new_geo_wkt()].
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
#' @param to A prototype to cast to. See [vctrs::vec_cast()]
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
  is_parseable <- cpp_validate_provider(x)
  stop_for_non_parseable(is_parseable)
  invisible(x)
}

#' @rdname new_geo_wkt
#' @export
vec_ptype_abbr.geo_wkt <- function(x, ...) {
  "wkt"
}

#' @rdname new_geo_wkt
#' @export
as_geo_wkt <- function(x, ...) {
  UseMethod("as_geo_wkt")
}

#' @rdname new_geo_wkt
#' @export
as_geo_wkt.default <- function(x, ...) {
  vec_cast(x, geo_wkt())
}

#' @rdname new_geo_wkt
#' @export
as_geo_wkt.character <- function(x, ...) {
  geo_wkt(x)
}

#' @method vec_cast geo_wkt
#' @export
#' @export vec_cast.geo_wkt
#' @rdname new_geo_wkt
vec_cast.geo_wkt <- function(x, to, ...) {
  UseMethod("vec_cast.geo_wkt")
}

#' @method vec_cast.geo_wkt default
#' @export
#' @rdname new_geo_wkt
vec_cast.geo_wkt.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_wkt geo_wkt
#' @export
#' @rdname new_geo_wkt
vec_cast.geo_wkt.geo_wkt <- function(x, to, ...) {
  x
}

#' @method vec_cast.geo_wkt character
#' @export
#' @rdname new_geo_wkt
vec_cast.geo_wkt.character <- function(x, to, ...) {
  as_geo_wkt.character(x)
}

#' @method vec_cast.character geo_wkt
#' @export
#' @rdname new_geo_wkt
vec_cast.character.geo_wkt <- function(x, to, ...) {
  vec_data(x)
}

#' @method vec_cast.geo_wkt geo_wkb
#' @export
#' @rdname new_geo_wkt
vec_cast.geo_wkt.geo_wkb <- function(x, to, ...) {
  new_geo_wkt(cpp_convert(x, new_geo_wkt()))
}
