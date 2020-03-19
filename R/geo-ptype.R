
#' Geometry-specific coersion
#'
#' Similar to [vctrs::vec_cast()] and [vctrs::vec_ptype()],
#' except that some important geometry data structures are
#' not strictly vctrs, in the sense that [vctrs::vec_size()]
#' may be different or not applicable (e.g., for a [geo_coord_linestring()]).
#' These functions help geometry operators accept a number of different
#' input and output formats. In particular, [geo_restore()] provides
#' any conversion that may be necessary between the C++ data structure
#' and the R data  structure.
#'
#' @param x A geometry-like object
#' @param to A prototype created by [geo_ptype()]
#'
#' @export
#'
geo_ptype <- function(x) {
  UseMethod("geo_ptype")
}

#' @rdname geo_ptype
#' @export
geo_ptype.default <- function(x) {
  vec_ptype(x)
}

#' @rdname geo_ptype
#' @export
geo_restore <- function(to, x) {
  UseMethod("geo_restore")
}

#' @rdname geo_ptype
#' @export
geo_restore.default <- function(to, x) {
  vec_restore(x, to)
}

#' @rdname geo_ptype
#' @export
geo_size <- function(x) {
  UseMethod("geo_size")
}

#' @rdname geo_ptype
#' @export
geo_size.default <- function(x) {
  vec_size(x)
}
