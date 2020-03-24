
#' @rdname geo_coord_point
#' @export
geo_coord_linestring <- function(xy, feature = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  tbl <- vec_recycle_common(
    xy = xy,
    feature = feature
  )

  validate_geo_coord_linestring(tbl)
  new_geo_coord_linestring(tbl)
}

#' @rdname geo_coord_point
#' @export
geo_coord_multilinestring <- function(xy, feature = 1L, part = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  part <- vec_cast(part, integer())
  tbl <- vec_recycle_common(
    xy = xy,
    feature = feature,
    part = part
  )

  validate_geo_coord_multilinestring(tbl)
  new_geo_coord_multilinestring(tbl)
}


#' S3 Details for (multi)linestring geometries
#'
#' @param x A (possibly) [geo_coord_linestring()] or [geo_coord_multilinestring()]
#' @param ... Unused
#'
#' @export
#'
new_geo_coord_linestring <- function(x = list(xy = geo_xy(), feature = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  new_rcrd(x, class = c("geo_coord_linestring", "geo_coord"))
}

#' @rdname new_geo_coord_linestring
#' @export
new_geo_coord_multilinestring <- function(x = list(xy = geo_xy(),
                                                 feature = integer(0),
                                                 part = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  vec_assert(x$part, integer())
  new_rcrd(x, class = c("geo_coord_multilinestring", "geo_coord"))
}

#' @rdname new_geo_coord_linestring
#' @export
validate_geo_coord_linestring <- function(x) {
  # Can't think of validation that is't already done in new*
  invisible(x)
}

#' @rdname new_geo_coord_linestring
#' @export
validate_geo_coord_multilinestring <- function(x) {
  # Can't think of any validation that isn't already done in new_*
  invisible(x)
}

#' @rdname new_geo_coord_linestring
#' @export
is_geo_coord_linestring <- function(x) {
  inherits(x, "geo_coord_linestring")
}

#' @rdname new_geo_coord_linestring
#' @export
is_geo_coord_multilinestring <- function(x) {
  inherits(x, "geo_coord_multilinestring")
}

#' @export
#' @rdname new_geo_coord_linestring
format.geo_coord_linestring <- function(x, ...) {
  format.geo_coord_point(x, ...)
}

#' @export
#' @rdname new_geo_coord_linestring
print.geo_coord_linestring <- function(x, ...) {
  print.geo_coord_point(x, ...)
}

#' @export
#' @rdname new_geo_coord_linestring
format.geo_coord_multilinestring <- function(x, ...) {
  sprintf(
    "<feat `%s.%s` %s>",
    field(x, "feature"),
    field(x, "part"),
    format(field(x, "xy"), ...)
  )
}

#' @export
#' @rdname new_geo_coord_linestring
print.geo_coord_multilinestring <- function(x, ...) {
  print.geo_coord_multipoint(x, ...)
}

#' @rdname new_geo_coord_linestring
#' @export
vec_ptype_abbr.geo_coord_linestring <- function(x, ...) {
  "tblls"
}

#' @rdname new_geo_coord_linestring
#' @export
vec_ptype_abbr.geo_coord_multilinestring <- function(x, ...) {
  "tblmls"
}

#' @rdname new_geo_coord_linestring
#' @export
as_geo_coord_linestring <- function(x, ...) {
  UseMethod("as_geo_coord_linestring")
}

#' @rdname new_geo_coord_linestring
#' @export
as_geo_coord_linestring.default <- function(x, ...) {
  vec_cast(x, new_geo_coord_linestring())
}

#' @rdname new_geo_coord_linestring
#' @export
as_geo_coord_multilinestring <- function(x, ...) {
  UseMethod("as_geo_coord_multilinestring")
}

#' @rdname new_geo_coord_linestring
#' @export
as_geo_coord_multilinestring.default <- function(x, ...) {
  vec_cast(x, new_geo_coord_multilinestring())
}
