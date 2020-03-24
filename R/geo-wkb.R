
#' Create and validate well-known binary
#'
#' Like other geo types, [geo_wkb()] doesn't convert its input
#' but does validate it using [validate_geo_wkb()].
#' To skip validation, use [new_geo_wkb()] with
#' the result of `vec_cast(list(...), .ptype = raw())`.
#'
#' @param x A [list()] of [raw()] objects, each of which
#'   represent well-known binary
#'
#' @return A [new_geo_wkb()]
#' @export
#'
#' @examples
#' # POINT (30 10) in WKB
#' wkb <- as.raw(
#'   c(
#'     0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x24, 0x40
#'   )
#' )
#' geo_wkb(list(wkb))
#'
geo_wkb <- function(x = list()) {
  x <- vec_cast(x,  list_of(.ptype = raw()))
  wkb <- validate_geo_wkb(new_geo_wkb(x))
  wkb
}


#' S3 details for geo_wkb
#'
#' @inheritParams geo_wkb
#' @param ... Unused
#' @param to A prototype to cast to. See [vctrs::vec_cast()]
#'
#' @export
#'
#' @examples
#' wkb_raw <- as.raw(
#'   c(
#'     0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
#'     0x00, 0x00, 0x00, 0x24, 0x40
#'   )
#' )
#' wkb <- geo_wkb(list(wkb_raw))
#' is_geo_wkb(wkb)
#'
new_geo_wkb <- function(x = vctrs::list_of(.ptype = raw())) {
  vec_assert(x, list_of(.ptype = raw()))
  new_list_of(x, raw(), class = c("geo_wkb", "geo"))
}

#' @rdname new_geo_wkb
#' @export
is_geo_wkb <- function(x) {
  inherits(x, "geo_wkb")
}

#' @rdname new_geo_wkb
#' @export
validate_geo_wkb <- function(x) {
  is_parseable <- cpp_validate_provider(x)
  stop_for_non_parseable(is_parseable)
  invisible(x)
}

#' @rdname new_geo_wkb
#' @export
vec_ptype_abbr.geo_wkb <- function(x, ...) {
  "wkb"
}

#' @export
#' @rdname new_geo_wkb
format.geo_wkb <- function(x, ...) {
  lengths <- vapply(x, length, integer(1))
  format(sprintf("<raw [%s]>", lengths), ...)
}

#' @export
#' @rdname new_geo_wkb
print.geo_wkb <- function(x, ...) {
  cat(
    paste0(
      sprintf("<geo_wkb [%s]>\n", length(x)),
      format(x, ...)
    )
  )

  invisible(x)
}

#' @export
#' @rdname new_geo_wkb
as_geo_wkb <- function(x, ...) {
  UseMethod("as_geo_wkb")
}

#' @export
#' @rdname new_geo_wkb
as_geo_wkb.default <- function(x, ...) {
  vec_cast(x, geo_wkb())
}

#' @method vec_cast geo_wkb
#' @export
#' @export vec_cast.geo_wkb
#' @rdname new_geo_wkb
vec_cast.geo_wkb <- function(x, to, ...) {
  UseMethod("vec_cast.geo_wkb")
}

#' @method vec_cast.geo_wkb default
#' @export
#' @rdname new_geo_wkb
vec_cast.geo_wkb.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_wkb geo_wkb
#' @export
#' @rdname new_geo_wkb
vec_cast.geo_wkb.geo_wkb <- function(x, to, ...) {
  x
}

#' @method vec_cast.geo_wkb list
#' @export
#' @rdname new_geo_wkb
vec_cast.geo_wkb.list <- function(x, to, ...) {
  geo_wkb(x)
}

#' @method vec_cast.geo_wkb geo_wkt
#' @export
#' @rdname new_geo_wkb
vec_cast.geo_wkb.geo_wkt <- function(x, to, ...) {
  new_geo_wkb(vec_cast(cpp_convert(x, new_geo_wkb()), list_of(raw())))
}
