
#' Create GEOS Geometry Vectors
#'
#' @param x An object to be coerced to a geometry vector
#' @param ... Unused
#'
#' @return A geos geometry vector
#' @export
#'
as_geos_geometry <- function(x, ...) {
  UseMethod("as_geos_geometry")
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.character <- function(x, ...) {
  new_geos_geometry(.Call(geos_c_read_wkt, x))
}

#' Create vectors of GEOS geometry objects
#'
#' @param x A bare `list()` of external pointers
#' @param class A character vector subclass
#' @param ... Unused
#'
#' @return An object of class geos_geometry
#' @noRd
#'
new_geos_geometry <- function(x = list(), class = character()) {
  if (!is.list(x) || is.object(x)) {
    stop("x must be a bare list of 'externalptr' objects")
  }

  structure(x, class = "geos_geometry")
}

validate_geos_geometry <- function(x) {
  type <- vapply(unclass(x), typeof, character(1))
  valid_items <- type %in% c("externalptr", "NULL")
  if (any(!valid_items)) {
    stop("Items must be externalptr objects or NULL")
  }

  invisible(x)
}

#' @export
is.na.geos_geometry <- function(x) {
  vapply(unclass(x), is.null, logical(1))
}

#' @export
`[.geos_geometry` <- function(x, i) {
  new_geos_geometry(NextMethod(), class(x))
}

# makes lapply() along these vectors possible
#' @export
`[[.geos_geometry` <- function(x, i) {
  x[i]
}

#' @export
`c.geos_geometry` <- function(...) {
  # make sure all items inherit the same top-level class
  dots <- list(...)
  inherits_first <- vapply(dots, inherits, "geos_geometry", FUN.VALUE = logical(1))
  if (!all(inherits_first)) {
    stop(sprintf("All items must inherit from 'geos_geometry'"), call. = FALSE)
  }

  geometry <- new_geos_geometry(NextMethod(), class(dots[[1]]))
  validate_geos_geometry(geometry)
  geometry
}

#' @export
rep.geos_geometry <- function(x, ...) {
  new_geos_geometry(NextMethod(), class(x))
}

#' @method rep_len geos_geometry
#' @export
rep_len.geos_geometry <- function(x, length.out) {
  rep(x, length.out = length.out)
}

#' @export
format.geos_geometry <- function(x, ...) {
  .Call(geos_c_write_wkt, x)
}

#' @export
print.geos_geometry <- function(x, ...) {
  cat(sprintf("<%s[%s]>\n", class(x)[1], length(x)))
  if (length(x) == 0) {
    return(invisible(x))
  }

  out <- stats::setNames(format(x, ...), names(x))
  print(out, quote = FALSE)
  invisible(x)
}
