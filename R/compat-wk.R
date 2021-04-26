
#' Compatibility with the wk package
#'
#' @inheritParams wk::wk_handle
#'
#' @return The result of the `handler`
#' @export
#' @rdname wk-methods
#' @name wk-methods
#'
#' @examples
#' library(wk)
#' wk_handle(as_geos_geometry("POINT (1 2)"), wk::wkt_writer())
#'
#' @importFrom wk wk_handle
wk_handle.geos_geometry <- function(handleable, handler, ...) {
  .Call(geos_c_wk_read_geos_geometry, handleable, handler)
}

#' @rdname wk-methods
#' @export
geos_geometry_writer <- function() {
  wk::new_wk_handler(.Call(geos_c_geos_writer_new), "geos_geometry_writer")
}

#' @rdname wk-methods
#' @importFrom wk wk_writer
#' @export
wk_writer.geos_geometry <- function(handleable, ...) {
  geos_geometry_writer()
}

#' @importFrom wk wk_crs
#' @export
wk_crs.geos_strtree <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @importFrom wk wk_crs
#' @export
wk_crs.geos_geometry <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @importFrom wk wk_set_crs
#' @export
wk_set_crs.geos_geometry <- function(x, crs) {
  attr(x, "crs") <- crs
  x
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_wkb <- function(x, ...) {
  geom <- wk_handle(x, geos_geometry_writer())
  attr(geom, "crs") <- attr(x, "crs", exact = TRUE)
  geom
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_wkt <- function(x, ...) {
  # geos_read_wkt() is faster, but doesn't do EWKT which might be
  # expected for users of wkt() since this is interpreted elsewhere
  geom <- wk_handle(x, geos_geometry_writer())
  attr(geom, "crs") <- attr(x, "crs", exact = TRUE)
  geom
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_xy <- function(x, ...) {
  x <- unclass(x)
  new_geos_geometry(
    .Call(geos_c_make_point, x[[1]], x[[2]], rep(NA_real_, length(x[[1]]))),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_xyz <- function(x, ...) {
  x <- unclass(x)
  new_geos_geometry(
    .Call(geos_c_make_point, x[[1]], x[[2]], x[[3]]),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_rct <- function(x, ...) {
  geom <- wk_handle(x, geos_geometry_writer())
  attr(geom, "crs") <- attr(x, "crs", exact = TRUE)
  geom
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_crc <- function(x, ...) {
  geom <- wk_handle(x, geos_geometry_writer())
  attr(geom, "crs") <- attr(x, "crs", exact = TRUE)
  geom
}

#' @importFrom wk as_wkt
#' @export
as_wkt.geos_geometry <- function(x, ..., include_z = TRUE, precision = 16, trim = TRUE) {
  wk::new_wk_wkt(
    geos_write_wkt(x, include_z = include_z, precision = precision, trim = trim),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @importFrom wk as_wkb
#' @export
as_wkb.geos_geometry <- function(x, ..., endian = NA_integer_) {
  geom <- wk_handle(x, wk::wkb_writer(endian = endian))
  attr(geom, "crs") <- attr(x, "crs", exact = TRUE)
  geom
}

#' @importFrom wk as_xy
#' @export
as_xy.geos_geometry <- function(x, ..., dims = NULL) {
  xy <- geos_write_xy(x)

  has_z <- geos_has_z(x)
  if (any(has_z, na.rm = TRUE) && (is.null(dims)) || ("z" %in% dims)) {
    is_empty <- is.na(x) | geos_is_empty(x)
    xy$z <- rep(NA_real_, length(x))
    xy$z[!is_empty] <- geos_z(x[!is_empty])
    wk::new_wk_xyz(xy, crs = attr(x, "crs", exact = TRUE))
  } else {
    wk::new_wk_xy(xy, crs = attr(x, "crs", exact = TRUE))
  }
}
