
#' Coordinate-long representations of geometries
#'
#' R's graphics use coordinate-long representations of geometries (i.e.,
#' one row per coordinate). Additionally, these can be more easily manipulated
#' than some other representations of geometry. **Important**: order matters
#' when passing a `geo_coord_*()` to other functions, which assume
#' that you use sequential `feature`, `piece`, and `part` values starting at 1.
#' Helpers to construct this particular representation of geometry from
#' a tibble are in the works.
#'
#' @param xy A [geo_xy()] (or something castable to [geo_xy()]) of coordinates
#' @param feature An integer ID that whose unique values can
#'   be used to identify and/or separate features.
#' @param piece An ID used to separate rings within polygons. The
#'   smallest `piece` value within a `feature`/`part` combination
#'   is assumed to be the outer ring.
#' @param part An ID used to separate parts within a feature of a multi* geometry
#'
#' @return A vctr
#' @export
#'
#' @examples
#' geo_coord_point(geo_xy(30, 10))
#' geo_coord_multipoint(geo_xy(1:4, 1:4), feature = c(1, 1, 2, 2))
#'
geo_coord_point <- function(xy, feature = seq_len(vec_size(xy))) {
  feature <- vec_cast(feature, integer())
  xy <- vec_cast(xy, geo_xy())
  tbl <- list(
    xy = xy,
    feature = rep_along_or_fail(feature, xy)
  )

  validate_geo_coord_point(tbl)
  new_geo_coord_point(tbl)
}

#' @rdname geo_coord_point
#' @export
geo_coord_multipoint <- function(xy, feature = seq_len(vec_size(xy))) {
  feature <- vec_cast(feature, integer())
  xy <- vec_cast(xy, geo_xy())
  tbl <- list(
    xy = xy,
    feature = rep_along_or_fail(feature, xy)
  )

  validate_geo_coord_multipoint(tbl)
  new_geo_coord_multipoint(tbl)
}

#' S3 Details for (multi)point geometries
#'
#' @param x A (possibly) [geo_coord_point()] or [geo_coord_multipoint()]
#' @param ... Unused
#'
#' @export
#'
new_geo_coord_point <- function(x = list(xy = geo_xy(), feature = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  new_rcrd(x, class = c("geo_coord_point", "geo_coord"))
}

#' @rdname new_geo_coord_point
#' @export
new_geo_coord_multipoint <- function(x = list(xy = geo_xy(), feature = integer(0))) {
  vec_assert(x$xy, geo_xy())
  vec_assert(x$feature, integer())
  new_rcrd(x, class = c("geo_coord_multipoint", "geo_coord"))
}

#' @rdname new_geo_coord_point
#' @export
validate_geo_coord_point <- function(x) {
  if (length(unique(field(x, "feature"))) != length(field(x, "feature"))) {
    abort(
      "Only one point per feature allowed in a geo_coord_point()\nDid you mean `geo_coord_multipoint()`?"
    )
  }

  invisible(x)
}

#' @rdname new_geo_coord_point
#' @export
validate_geo_coord_multipoint <- function(x) {
  # Can't think of any validation that isn't already done in new_*
  invisible(x)
}

#' @rdname new_geo_coord_point
#' @export
is_geo_coord_point <- function(x) {
  inherits(x, "geo_coord_point")
}

#' @rdname new_geo_coord_point
#' @export
is_geo_coord_multipoint <- function(x) {
  inherits(x, "geo_coord_multipoint")
}

#' @export
#' @rdname new_geo_coord_point
format.geo_coord_point <- function(x, ...) {
  sprintf("<feat `%s` %s>", field(x, "feature"), format(field(x, "xy"), ...))
}

#' @export
#' @rdname new_geo_coord_point
print.geo_coord_point <- function(x, ...) {
  cat(
    sprintf(
      "<%s [%s coords, %s features]>\n",
      class(x)[1], vec_size(x), geo_size(x)
    )
  )
  print(format(x), ..., quote = FALSE)
  invisible(x)
}

#' @export
#' @rdname new_geo_coord_point
format.geo_coord_multipoint <- function(x, ...) {
  sprintf("<feat `%s.1` %s>", field(x, "feature"), format(field(x, "xy"), ...))
}

#' @export
#' @rdname new_geo_coord_point
print.geo_coord_multipoint <- function(x, ...) {
  print.geo_coord_point(x, ...)
}

#' @rdname new_geo_coord_point
#' @export
vec_ptype_abbr.geo_coord_point <- function(x, ...) {
  "tblpnt"
}

#' @rdname new_geo_coord_point
#' @export
vec_ptype_abbr.geo_coord_multipoint <- function(x, ...) {
  "tblmpnt"
}

#' @rdname new_geo_coord_point
#' @export
as_geo_coord_point <- function(x, ...) {
  UseMethod("as_geo_coord_point")
}

#' @rdname new_geo_coord_point
#' @export
as_geo_coord_point.default <- function(x, ...) {
  vec_cast(x, new_geo_coord_point())
}

#' @rdname new_geo_coord_point
#' @export
as_geo_coord_multipoint <- function(x, ...) {
  UseMethod("as_geo_coord_multipoint")
}

#' @rdname new_geo_coord_point
#' @export
as_geo_coord_multipoint.default <- function(x, ...) {
  vec_cast(x, new_geo_coord_multipoint())
}
