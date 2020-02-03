
#' Create and validate well-known binary
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
  new_geo_wkb(x)
}


#' S3 details for geo_wkb
#'
#' @inheritParams geo_wkb
#' @param ... Unused
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
  abort("Not implemented")
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
