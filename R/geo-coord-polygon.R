
#' @rdname geo_coord_point
#' @export
geo_coord_polygon <- function(xy, feature = 1L, piece = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  piece <- vec_cast(piece, integer())
  tbl <- vec_recycle_common(
    xy = xy,
    feature = feature,
    piece = piece
  )

  validate_geo_coord_polygon(tbl)
  new_geo_coord_polygon(tbl)
}

#' @rdname geo_coord_point
#' @export
geo_coord_multipolygon <- function(xy, feature = 1L, piece = 1L, part = 1L) {
  xy <- vec_cast(xy, geo_xy())
  feature <- vec_cast(feature, integer())
  piece <- vec_cast(piece, integer())
  part <- vec_cast(part, integer())
  tbl <- vec_recycle_common(
    xy = xy,
    feature = feature,
    part = part,
    piece = piece
  )

  validate_geo_coord_multipolygon(tbl)
  new_geo_coord_multipolygon(tbl)
}


#' S3 Details for (multi)polygon geometries
#'
#' @param x A (possibly) [geo_coord_polygon()] or [geo_coord_multipolygon()]
#' @param ... Unused
#'
#' @export
#'
new_geo_coord_polygon <- function(x = list(xy = geo_xy(), feature = integer(0), piece = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$piece, integer())
  vec_assert(x$feature, integer())
  new_rcrd(x, class = c("geo_coord_polygon", "geo_coord"))
}

#' @rdname new_geo_coord_polygon
#' @export
new_geo_coord_multipolygon <- function(x = list(xy = geo_xy(),
                                              feature = integer(0),
                                              part = integer(0),
                                              piece = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  vec_assert(x$part, integer())
  vec_assert(x$piece, integer())
  new_rcrd(x, class = c("geo_coord_multipolygon", "geo_coord"))
}

#' @rdname new_geo_coord_polygon
#' @export
validate_geo_coord_polygon <- function(x) {
  # Can't think of validation that is't already done in new*
  invisible(x)
}

#' @rdname new_geo_coord_polygon
#' @export
validate_geo_coord_multipolygon <- function(x) {
  # Can't think of any validation that isn't already done in new_*
  invisible(x)
}

#' @rdname new_geo_coord_polygon
#' @export
is_geo_coord_polygon <- function(x) {
  inherits(x, "geo_coord_polygon")
}

#' @rdname new_geo_coord_polygon
#' @export
is_geo_coord_multipolygon <- function(x) {
  inherits(x, "geo_coord_multipolygon")
}

#' @export
#' @rdname new_geo_coord_polygon
format.geo_coord_polygon <- function(x, ...) {
  format.geo_coord_linestring(x, ...)
}

#' @export
#' @rdname new_geo_coord_polygon
print.geo_coord_polygon <- function(x, ...) {
  print.geo_coord_linestring(x, ...)
}

#' @export
#' @rdname new_geo_coord_polygon
format.geo_coord_multipolygon <- function(x, ...) {
  format.geo_coord_multilinestring(x, ...)
}

#' @export
#' @rdname new_geo_coord_polygon
print.geo_coord_multipolygon <- function(x, ...) {
  print.geo_coord_multilinestring(x, ...)
}

#' @rdname new_geo_coord_polygon
#' @export
vec_ptype_abbr.geo_coord_polygon <- function(x, ...) {
  "tblply"
}

#' @rdname new_geo_coord_polygon
#' @export
vec_ptype_abbr.geo_coord_multipolygon <- function(x, ...) {
  "tblmply"
}

#' @rdname new_geo_coord_polygon
#' @export
as_geo_coord_polygon <- function(x, ...) {
  UseMethod("as_geo_coord_polygon")
}

#' @rdname new_geo_coord_polygon
#' @export
as_geo_coord_polygon.default <- function(x, ...) {
  vec_cast(x, new_geo_coord_polygon())
}

#' @rdname new_geo_coord_polygon
#' @export
as_geo_coord_multipolygon <- function(x, ...) {
  UseMethod("as_geo_coord_multipolygon")
}

#' @rdname new_geo_coord_polygon
#' @export
as_geo_coord_multipolygon.default <- function(x, ...) {
  vec_cast(x, new_geo_coord_multipolygon())
}

