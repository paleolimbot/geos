
#' @rdname geo_tbl_point
#' @export
geo_tbl_linestring <- function(xy, feature = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  tbl <- list(xy = xy, feature = rep_len_or_fail(feature, xy))

  validate_geo_tbl_linestring(tbl)
  new_geo_tbl_linestring(tbl)
}

#' @rdname geo_tbl_point
#' @export
geo_tbl_multilinestring <- function(xy, feature = 1L, part = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  tbl <- list(
    xy = xy,
    feature = rep_len_or_fail(feature, xy),
    part = rep_len_or_fail(part, xy)
  )

  validate_geo_tbl_multilinestring(tbl)
  new_geo_tbl_multilinestring(tbl)
}


#' S3 Details for (multi)linestring geometries
#'
#' @param x A (possibly) [geo_tbl_linestring()] or [geo_tbl_multilinestring()]
#' @param ... Unused
#'
#' @export
#'
new_geo_tbl_linestring <- function(x = list(xy = geo_xy(), feature = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  new_rcrd(x, class = "geo_tbl_linestring")
}

#' @rdname new_geo_tbl_linestring
#' @export
new_geo_tbl_multilinestring <- function(x = list(xy = geo_xy(), feature = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  vec_assert(x$part, integer())
  new_rcrd(x, class = "geo_tbl_multilinestring")
}

#' @rdname new_geo_tbl_linestring
#' @export
validate_geo_tbl_linestring <- function(x) {
  # Can't think of validation that is't already done in new*
  invisible(x)
}

#' @rdname new_geo_tbl_linestring
#' @export
validate_geo_tbl_multilinestring <- function(x) {
  # Can't think of any validation that isn't already done in new_*
  invisible(x)
}

#' @rdname new_geo_tbl_linestring
#' @export
is_geo_tbl_linestring <- function(x) {
  inherits(x, "geo_tbl_linestring")
}

#' @rdname new_geo_tbl_linestring
#' @export
is_geo_tbl_multilinestring <- function(x) {
  inherits(x, "geo_tbl_multilinestring")
}

#' @export
#' @rdname new_geo_tbl_linestring
format.geo_tbl_linestring <- function(x, ...) {
  format.geo_tbl_point(x, ...)
}

#' @export
#' @rdname new_geo_tbl_linestring
print.geo_tbl_linestring <- function(x, ...) {
  print.geo_tbl_point(x, ...)
}

#' @export
#' @rdname new_geo_tbl_linestring
format.geo_tbl_multilinestring <- function(x, ...) {
  sprintf(
    "<feat `%s.%s` %s>",
    field(x, "feature"),
    field(x, "part"),
    format(field(x, "xy"), ...)
  )
}

#' @export
#' @rdname new_geo_tbl_linestring
print.geo_tbl_multilinestring <- function(x, ...) {
  print.geo_tbl_multipoint(x, ...)
}

#' @rdname new_geo_tbl_linestring
#' @export
vec_ptype_abbr.geo_tbl_linestring <- function(x, ...) {
  "tblls"
}

#' @rdname new_geo_tbl_linestring
#' @export
vec_ptype_abbr.geo_tbl_multilinestring <- function(x, ...) {
  "tblmls"
}
