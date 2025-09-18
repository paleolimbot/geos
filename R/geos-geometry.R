#' Create GEOS Geometry Vectors
#'
#' @param x An object to be coerced to a geometry vector
#' @param crs An object that can be interpreted as a CRS. See [wk::wk_crs()].
#' @param ... Unused
#'
#' @return A geos geometry vector
#' @export
#'
#' @examples
#' as_geos_geometry("LINESTRING (0 1, 3 9)")
#'
as_geos_geometry <- function(x, ...) {
  UseMethod("as_geos_geometry")
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.geos_geometry <- function(x, ...) {
  x
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.default <- function(x, ...) {
  if (wk::wk_is_geodesic(x)) {
    stop("Can't convert geometry with geodesic edges to geos_geometry()")
  }

  wk::wk_translate(x, geos_geometry())
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.character <- function(x, ..., crs = NULL) {
  geos_read_wkt(x, crs = crs)
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.blob <- function(x, ..., crs = NULL) {
  attributes(x) <- NULL
  geom <- wk_handle(wk::new_wk_wkb(x, crs = NULL), geos_geometry_writer())
  attr(geom, "crs") <- crs
  geom
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.WKB <- function(x, ..., crs = NULL) {
  attributes(x) <- NULL
  geom <- wk_handle(wk::new_wk_wkb(x, crs = NULL), geos_geometry_writer())
  attr(geom, "crs") <- crs
  geom
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.SpatVector <- function(x, ...) {
  geos::as_geos_geometry(
    terra::geom(x, wkt = TRUE),
    crs = get_terra_crs(x)
  )
}

#' @rdname as_geos_geometry
#' @export
geos_geometry <- function(crs = wk::wk_crs_inherit()) {
  new_geos_geometry(list(), crs = crs)
}

new_geos_geometry <- function(x = list(), crs = NULL) {
  if (!is.list(x) || is.object(x)) {
    stop("x must be a bare list of 'externalptr' objects")
  }

  structure(x, class = "geos_geometry", crs = crs)
}

validate_geos_geometry <- function(x) {
  valid_items <- .Call(geos_c_geos_geometry_is_null_or_xptr, x)
  if (any(!valid_items)) {
    stop("Items must be externalptr objects or NULL")
  }

  invisible(x)
}

#' @export
is.na.geos_geometry <- function(x) {
  .Call(geos_c_geos_geometry_is_null, x)
}

#' @export
`[.geos_geometry` <- function(x, i) {
  new_geos_geometry(NextMethod(), crs = attr(x, "crs", exact = TRUE))
}

# makes lapply() along these vectors possible
#' @export
`[[.geos_geometry` <- function(x, i) {
  x[i]
}

#' @export
`[<-.geos_geometry` <- function(x, i, value) {
  replacement <- as_geos_geometry(value)
  crs_out <- wk_crs_output(x, replacement)
  x <- unclass(x)
  x[i] <- replacement
  attr(x, "crs") <- NULL
  new_geos_geometry(x, crs = crs_out)
}

#' @export
`[[<-.geos_geometry` <- function(x, i, value) {
  x[i] <- value
  x
}

#' @export
`c.geos_geometry` <- function(...) {
  # make sure all items inherit the same top-level class
  dots <- list(...)
  inherits_first <- vapply(
    dots,
    inherits,
    "geos_geometry",
    FUN.VALUE = logical(1)
  )
  if (!all(inherits_first)) {
    stop(
      sprintf("All items in c(...) must inherit from 'geos_geometry'"),
      call. = FALSE
    )
  }

  # check CRS compatibility
  crs_reduce <- function(x, y) new_geos_geometry(crs = wk_crs_output(x, y))
  crs <- Reduce(crs_reduce, dots)

  geometry <- new_geos_geometry(
    NextMethod(),
    crs = attr(dots[[1]], "crs", exact = TRUE)
  )
  validate_geos_geometry(geometry)
  geometry
}

#' @export
rep.geos_geometry <- function(x, ...) {
  new_geos_geometry(unclass(NextMethod()), crs = attr(x, "crs", exact = TRUE))
}

#' @method rep_len geos_geometry
#' @export
rep_len.geos_geometry <- function(x, length.out) {
  rep(x, length.out = length.out)
}

#' @export
format.geos_geometry <- function(x, ..., precision = 5, max_coords = 5) {
  # by default, use bounds and type because this is fast even for huge
  # geometries

  # min/max functions do not work on EMPTY, so can't run geos_xmin/max()
  # on these geometries
  n_coords <- geos_num_coordinates(x)
  use_wkt <- is.na(x) | (n_coords <= max_coords)

  formatted <- rep_len("", length(x))
  formatted[use_wkt] <- geos_write_wkt(x[use_wkt], precision = precision)

  formatted[!use_wkt] <- sprintf(
    "%s [%s %s...%s %s]",
    toupper(geos_type(x[!use_wkt])),
    format(geos_xmin(x[!use_wkt]), trim = TRUE, digits = precision),
    format(geos_ymin(x[!use_wkt]), trim = TRUE, digits = precision),
    format(geos_xmax(x[!use_wkt]), trim = TRUE, digits = precision),
    format(geos_ymax(x[!use_wkt]), trim = TRUE, digits = precision)
  )

  sprintf("<%s>", formatted)
}

# this is what shows up in the RStudio viewer, which should be
# fast to calculate even for huge geometries
#' @export
as.character.geos_geometry <- function(x, ...) {
  format(x, ...)
}

# data.frame() will call as.data.frame() with optional = TRUE
#' @export
as.data.frame.geos_geometry <- function(x, ..., optional = FALSE) {
  if (!optional) {
    NextMethod()
  } else {
    new_data_frame(list(x))
  }
}

#' @export
print.geos_geometry <- function(x, ...) {
  crs <- attr(x, "crs", exact = TRUE)
  if (is.null(crs)) {
    cat(sprintf("<%s[%s]>\n", class(x)[1], length(x)))
  } else {
    cat(sprintf("<%s[%s] with CRS=%s>\n", class(x)[1], length(x), format(crs)))
  }

  if (length(x) == 0) {
    return(invisible(x))
  }

  out <- stats::setNames(format(x, ...), names(x))
  print(out, quote = FALSE)
  invisible(x)
}

# lifted from vctrs::obj_leaf()
#' @export
str.geos_geometry <- function(
    object,
    ...,
    indent.str = "",
    width = getOption("width")) {
  if (length(object) == 0) {
    cat(paste0(" ", class(object)[1], "[0]\n"))
    return(invisible(object))
  }

  # estimate possible number of elements that could be displayed
  # to avoid formatting too many
  width <- width - nchar(indent.str) - 2
  length <- min(length(object), ceiling(width / 5))
  formatted <- format(object[seq_len(length)], trim = TRUE)

  title <- paste0(" ", class(object)[1], "[1:", length(object), "]")
  cat(
    paste0(
      title,
      " ",
      strtrim(paste0(formatted, collapse = ", "), width - nchar(title)),
      "\n"
    )
  )
  invisible(object)
}

#' Helper function for CRS extraction from SpatVector objects
#' @noRd
#' @keywords internal
get_terra_crs <- function(x) {
  crs <- list(
    input = terra::crs(x, describe = TRUE)$name,
    wkt = terra::crs(x)
  )
  attr(crs, "class") <- "crs"
  crs
}
