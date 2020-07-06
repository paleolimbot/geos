
#' Create vectors of XPtr objects
#'
#' @param x A bare `list()` of external pointers
#' @param class A character vector subclass
#' @param ... Unused
#'
#' @return An object of class geos_xptr
#' @noRd
#'
new_geos_xptr <- function(x = list(), class = character()) {
  if (!is.list(x) || is.object(x)) {
    stop("x must be a bare list of 'externalptr' objects")
  }

  structure(x, class = "geos_xptr")
}

validate_geos_xptr <- function(x) {
  type <- vapply(unclass(x), typeof, character(1))
  valid_items <- type %in% c("externalptr", "NULL")
  if (any(!valid_items)) {
    stop("Items must be externalptr objects or NULL")
  }

  invisible(x)
}

#' @export
`[.geos_xptr` <- function(x, i) {
  new_geos_xptr(NextMethod(), class(x))
}

# makes lapply() along these vectors possible
#' @export
`[[.geos_xptr` <- function(x, i) {
  x[i]
}

#' @export
`c.geos_xptr` <- function(...) {
  # make sure all items inherit the same top-level class
  dots <- list(...)
  inherits_first <- vapply(dots, inherits, "geos_xptr", FUN.VALUE = logical(1))
  if (!all(inherits_first)) {
    stop(sprintf("All items must inherit from 'geos_xptr'"), call. = FALSE)
  }

  xptr <- new_geos_xptr(NextMethod(), class(dots[[1]]))
  validate_geos_xptr(xptr)
  xptr
}

#' @export
rep.geos_xptr <- function(x, ...) {
  new_geos_xptr(NextMethod(), class(x))
}

#' @method rep_len geos_xptr
#' @export
rep_len.geos_xptr <- function(x, length.out) {
  rep(x, length.out = length.out)
}

#' @export
print.geos_xptr <- function(x, ...) {
  cat(sprintf("<%s[%s]>\n", class(x)[1], length(x)))
  if (length(x) == 0) {
    return(invisible(x))
  }

  out <- stats::setNames(format(x, ...), names(x))
  print(out, quote = FALSE)
  invisible(x)
}
