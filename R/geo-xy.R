
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
#' @param x A (possibly) [geo_xy()]
#' @param y,to,x_arg,y_arg See [vctrs::vec_cast()] and [vctrs::vec_ptype2()]
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

#' @export
#' @rdname new_geo_xy
as.data.frame.geo_xy <- function(x, ...) {
  new_data_frame(x)
}

#' @export
#' @rdname new_geo_xy
as_tibble.geo_xy <- function(x, ...) {
  as_tibble(vec_data(x))
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
as_geo_xy.list <- function(x, ...) {
  if (rlang::is_dictionaryish(x)) {
    geo_xy(x$x, x$y)
  } else {
    abort("Can't cast an unnamed list to <geo_xy>")
  }
}

#' @method vec_ptype2 geo_xy
#' @export
#' @export vec_ptype2.geo_xy
#' @rdname new_geo_xy
vec_ptype2.geo_xy <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_xy", y)
}

#' @method vec_ptype2.geo_xy default
#' @export
#' @rdname new_geo_xy
vec_ptype2.geo_xy.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_xy data.frame
#' @export
#' @rdname new_geo_xy
vec_ptype2.geo_xy.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  tibble(x = double(), y = double())
}

#' @method vec_ptype2.data.frame geo_xy
#' @export
#' @rdname new_geo_xy
vec_ptype2.data.frame.geo_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  tibble(x = double(), y = double())
}

#' @method vec_cast geo_xy
#' @export
#' @export vec_cast.geo_xy
#' @rdname new_geo_xy
vec_cast.geo_xy <- function(x, to, ...) {
  UseMethod("vec_cast.geo_xy")
}

#' @method vec_cast.geo_xy default
#' @export
#' @rdname new_geo_xy
vec_cast.geo_xy.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_xy data.frame
#' @export
#' @rdname new_geo_xy
vec_cast.geo_xy.data.frame <- function(x, to, ...) {
  geo_xy(x$x, x$y)
}

#' @method vec_cast.data.frame geo_xy
#' @export
#' @rdname new_geo_xy
vec_cast.data.frame.geo_xy <- function(x, to, ...) {
  as_tibble(x)
}

#' @method vec_cast.geo_xy list
#' @export
#' @rdname new_geo_xy
vec_cast.geo_xy.list <- function(x, to, ...) {
  as_geo_xy.list(x)
}

#' @method vec_cast.list geo_xy
#' @export
#' @rdname new_geo_xy
vec_cast.list.geo_xy <- function(x, to, ...) {
  vec_data(x)
}
