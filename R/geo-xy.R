
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
  new_geo_xy(vec_recycle_common(x = vec_cast(x, double()), y = vec_cast(y, double())))
}

#' S3 details for geo_xy
#'
#' @param x A (possibly) [geo_xy()]
#' @param ... Unused
#'
#' @export
#'
new_geo_xy <- function(x = list(x = double(), y = double())) {
  vec_assert(x$x, double())
  vec_assert(x$y, double())
  new_rcrd(x, class = c("geo_xy", "geo_coord"))
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

#' @export
#' @rdname new_geo_xy
as_geo_xy <- function(x, ...) {
  UseMethod("as_geo_xy")
}

#' @export
#' @rdname new_geo_xy
as_geo_xy.default <- function(x, ...) {
  vec_cast(x, geo_xy())
}

#' @export
#' @rdname new_geo_xy
as_geo_xy.matrix <- function(x, ...) {
  names <- colnames(x)
  if (all(c("x", "y") %in% names)) {
    x_col <- match("x", names)
    y_col <- match("y", names)
  } else {
    x_col <- 1
    y_col <- 2
  }

  geo_xy(x = x[, x_col, drop = TRUE], y = x[, y_col, drop = TRUE])
}

#' @export
#' @rdname new_geo_xy
as.matrix.geo_xy <- function(x, ...) {
  as.matrix(as.data.frame(x))
}
