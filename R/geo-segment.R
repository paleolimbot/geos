
#' Line segments
#'
#' @param start,end [geo_xy()]s for the start and end
#'   of the segment, respectively.
#'
#' @return A [new_geo_segment()]
#' @export
#'
#' @examples
#' geo_segment(start = geo_xy(), end = geo_xy())
#'
geo_segment <- function(start = geo_xy(), end = geo_xy()) {
  result <- new_geo_segment(
    vec_recycle_common(
      start = vec_cast(start, geo_xy()),
      end = vec_cast(end, geo_xy())
    )
  )

  result
}

#' S3 details for geo_segment
#'
#' @param x A (possibly) [geo_segment()]
#' @param ... Unused
#'
#' @export
#'
new_geo_segment <- function(x = list(start = geo_xy(), end = geo_xy())) {
  vec_assert(x$start, geo_xy())
  vec_assert(x$end, geo_xy())
  new_rcrd(x, class = c("geo_segment", "geo_coord"))
}

#' @export
#' @rdname new_geo_segment
is_geo_segment <- function(x) {
  inherits(x, "geo_segment")
}

#' @export
#' @rdname new_geo_segment
validate_geo_segment <- function(x) {
  abort("not implemented")
}

#' @rdname new_geo_segment
#' @export
vec_ptype_abbr.geo_segment <- function(x, ...) {
  "segment"
}

#' @export
#' @rdname new_geo_segment
format.geo_segment <- function(x, ...) {
  sprintf(
    "(%s %s)=>(%s %s)",
    format(field(field(x, "start"), "x"), trim = TRUE, ...),
    format(field(field(x, "start"), "y"), trim = TRUE, ...),
    format(field(field(x, "end"), "x"), trim = TRUE, ...),
    format(field(field(x, "end"), "y"), trim = TRUE, ...)
  )
}

#' @export
#' @rdname new_geo_segment
as_geo_segment <- function(x, ...) {
  UseMethod("as_geo_segment")
}

#' @export
#' @rdname new_geo_segment
as_geo_segment.default <- function(x, ...) {
  vec_cast(x, geo_segment())
}

#' @export
#' @rdname new_geo_segment
geo_size.geo_segment <- function(x, ...) {
  vec_size(x)
}

#' @export
geo_ptype.geo_segment <- function(x, ...) {
  # returning a geo_wkt() here means that we can use
  # a geo_segment() as an input to geometry functions without
  # getting only getting bboxes back
  geo_wkt()
}

#' @export
geo_restore.geo_segment <- function(to, x) {
  x
}
