
#' Create a coordinate vector
#'
#' @param x,y x and y coordinates
#'
#' @return A [new_geo_xy()]
#' @export
#'
#' @examples
#' geo_xy(0:5, 1:6)
#'
geo_xy <- function(x = double(), y = double()) {
  new_geo_xy(list(x = vec_cast(x, double()), y = vec_cast(y, double())))
}

#' S3 details for geo_xy
#'
#' @param x A (possibly) [geo_xy()].
#' @param ... Unused
#'
#' @export
#'
#' @examples
#' xy <- geo_xy(0:5, 1:6)
#' is_geo_xy(xy)
#'
new_geo_xy <- function(x = list(x = double(), y = double())) {
  vec_assert(x$x, double())
  vec_assert(x$y, double())
  new_rcrd(x, class = c("geo_xy", "geo"))
}

#' @export
#' @rdname new_geo_xy
is_geo_xy <- function(x) {
  inherits(x, "geo_xy")
}

#' @export
#' @rdname new_geo_xy
validate_geo_xy <- function(x) {
  abort("not implemented")
}

#' @rdname new_geo_xy
#' @export
vec_ptype_abbr.geo_xy <- function(x, ...) {
  "xy"
}

#' @export
#' @rdname new_geo_xy
format.geo_xy <- function(x, ...) {
  sprintf(
    "(%s %s)",
    format(field(x, "x"), trim = TRUE, ...),
    format(field(x, "y"), trim = TRUE, ...)
  )
}
