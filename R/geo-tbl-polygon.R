
#' @rdname geo_tbl_point
#' @export
geo_tbl_polygon <- function(xy, feature = 1L, piece = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  piece <- vec_cast(piece, integer())
  tbl <- list(
    xy = xy,
    feature = rep_len_or_fail(feature, xy),
    piece = rep_len_or_fail(piece, xy)
  )

  validate_geo_tbl_polygon(tbl)
  new_geo_tbl_polygon(tbl)
}

#' @rdname geo_tbl_point
#' @export
geo_tbl_multipolygon <- function(xy, feature = 1L, piece = 1L, part = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  piece <- vec_cast(piece, integer())
  part <- vec_cast(part, integer())
  tbl <- list(
    xy = xy,
    feature = rep_len_or_fail(feature, xy),
    part = rep_len_or_fail(part, xy),
    piece = rep_len_or_fail(piece, xy)
  )

  validate_geo_tbl_multipolygon(tbl)
  new_geo_tbl_multipolygon(tbl)
}


#' S3 Details for (multi)polygon geometries
#'
#' @param x A (possibly) [geo_tbl_polygon()] or [geo_tbl_multipolygon()]
#' @param ... Unused
#'
#' @export
#'
new_geo_tbl_polygon <- function(x = list(xy = geo_xy(), feature = integer(0), piece = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$piece, integer())
  vec_assert(x$feature, integer())
  new_rcrd(x, class = c("geo_tbl_polygon", "geo_tbl"))
}

#' @rdname new_geo_tbl_polygon
#' @export
new_geo_tbl_multipolygon <- function(x = list(xy = geo_xy(),
                                              feature = integer(0),
                                              part = integer(0),
                                              piece = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  vec_assert(x$part, integer())
  vec_assert(x$piece, integer())
  new_rcrd(x, class = c("geo_tbl_multipolygon", "geo_tbl"))
}

#' @rdname new_geo_tbl_polygon
#' @export
validate_geo_tbl_polygon <- function(x) {
  # Can't think of validation that is't already done in new*
  invisible(x)
}

#' @rdname new_geo_tbl_polygon
#' @export
validate_geo_tbl_multipolygon <- function(x) {
  # Can't think of any validation that isn't already done in new_*
  invisible(x)
}

#' @rdname new_geo_tbl_polygon
#' @export
is_geo_tbl_polygon <- function(x) {
  inherits(x, "geo_tbl_polygon")
}

#' @rdname new_geo_tbl_polygon
#' @export
is_geo_tbl_multipolygon <- function(x) {
  inherits(x, "geo_tbl_multipolygon")
}

#' @export
#' @rdname new_geo_tbl_polygon
format.geo_tbl_polygon <- function(x, ...) {
  format.geo_tbl_linestring(x, ...)
}

#' @export
#' @rdname new_geo_tbl_polygon
print.geo_tbl_polygon <- function(x, ...) {
  print.geo_tbl_linestring(x, ...)
}

#' @export
#' @rdname new_geo_tbl_polygon
format.geo_tbl_multipolygon <- function(x, ...) {
  format.geo_tbl_multilinestring(x, ...)
}

#' @export
#' @rdname new_geo_tbl_polygon
print.geo_tbl_multipolygon <- function(x, ...) {
  print.geo_tbl_multilinestring(x, ...)
}

#' @rdname new_geo_tbl_polygon
#' @export
vec_ptype_abbr.geo_tbl_polygon <- function(x, ...) {
  "tblply"
}

#' @rdname new_geo_tbl_polygon
#' @export
vec_ptype_abbr.geo_tbl_multipolygon <- function(x, ...) {
  "tblmply"
}

#' @rdname new_geo_tbl_polygon
#' @export
as_geo_tbl_polygon <- function(x, ...) {
  UseMethod("as_geo_tbl_polygon")
}

#' @rdname new_geo_tbl_polygon
#' @export
as_geo_tbl_polygon.default <- function(x, ...) {
  vec_cast(x, new_geo_tbl_polygon())
}

#' @rdname new_geo_tbl_polygon
#' @export
as_geo_tbl_multipolygon <- function(x, ...) {
  UseMethod("as_geo_tbl_multipolygon")
}

#' @rdname new_geo_tbl_polygon
#' @export
as_geo_tbl_multipolygon.default <- function(x, ...) {
  vec_cast(x, new_geo_tbl_multipolygon())
}

