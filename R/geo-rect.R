
#' Rectangles
#'
#' @param xmin,ymin,xmax,ymax Border values, recycled to a common
#'   length using [vctrs::vec_recycle_common()].
#'
#' @return A [new_geo_rect()]
#' @export
#'
#' @examples
#' geo_rect(xmin = 0:5, ymin = 0:5, xmax = 2:7, ymax = 2:7)
#'
geo_rect <- function(xmin = double(), ymin = double(), xmax = double(), ymax = double()) {
  result <- new_geo_rect(
    vec_recycle_common(
      xmin = vec_cast(xmin, double()),
      ymin = vec_cast(ymin, double()),
      xmax = vec_cast(xmax, double()),
      ymax = vec_cast(ymax, double())
    )
  )

  result
}

#' S3 details for geo_rect
#'
#' @param x A (possibly) [geo_rect()]
#' @param ... Unused
#'
#' @export
#'
new_geo_rect <- function(x = list(xmin = double(), ymin = double(), xmax = double(), ymax = double())) {
  vec_assert(x$xmin, double())
  vec_assert(x$ymin, double())
  vec_assert(x$xmax, double())
  vec_assert(x$ymax, double())
  new_rcrd(x, class = c("geo_rect", "geo_coord"))
}

#' @export
#' @rdname new_geo_rect
is_geo_rect <- function(x) {
  inherits(x, "geo_rect")
}

#' @export
#' @rdname new_geo_rect
validate_geo_rect <- function(x) {
  abort("not implemented")
}

#' @rdname new_geo_rect
#' @export
vec_ptype_abbr.geo_rect <- function(x, ...) {
  "rect"
}

#' @export
#' @rdname new_geo_rect
format.geo_rect <- function(x, ...) {
  sprintf(
    "(%s %s)x(%s %s)",
    format(field(x, "xmin"), trim = TRUE, ...),
    format(field(x, "ymin"), trim = TRUE, ...),
    format(field(x, "xmax"), trim = TRUE, ...),
    format(field(x, "ymax"), trim = TRUE, ...)
  )
}

#' @export
#' @rdname new_geo_rect
as_geo_rect <- function(x, ...) {
  UseMethod("as_geo_rect")
}

#' @export
#' @rdname new_geo_rect
as_geo_rect.default <- function(x, ...) {
  vec_cast(x, geo_rect())
}

#' @export
#' @rdname new_geo_rect
as_geo_rect.matrix <- function(x, ...) {
  names <- colnames(x)
  if (all(c("xmin", "ymin", "xmax", "ymax") %in% names)) {
    xmin_col <- match("xmin", names)
    ymin_col <- match("ymin", names)
    xmax_col <- match("xmax", names)
    ymax_col <- match("ymax", names)
  } else {
    xmin_col <- 1
    ymin_col <- 2
    xmax_col <- 3
    ymax_col <- 4
  }

  geo_rect(
    xmin = x[, xmin_col, drop = TRUE],
    ymin = x[, ymin_col, drop = TRUE],
    xmax = x[, xmax_col, drop = TRUE],
    ymax = x[, ymax_col, drop = TRUE]
  )
}

#' @export
#' @rdname new_geo_rect
as.matrix.geo_rect <- function(x, ...) {
  as.matrix(as.data.frame(x))
}

#' @export
#' @rdname new_geo_rect
geo_size.geo_rect <- function(x, ...) {
  vec_size(x)
}

#' @export
geo_ptype.geo_rect <- function(x, ...) {
  # returning a geo_wkt() here means that we can use
  # a geo_rect() as an input to geometry functions without
  # getting only getting bboxes back
  geo_wkt()
}

#' @export
geo_restore.geo_rect <- function(to, x) {
  x
}
