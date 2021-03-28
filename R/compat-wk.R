
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
  geos_read_wkb(x, crs = attr(x, "crs", exact = TRUE))
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_wkt <- function(x, ...) {
  geos_read_wkt(x, attr(x, "crs", exact = TRUE))
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
  as_geos_geometry(wk::as_wkb(x, ...))
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.wk_crc <- function(x, ...) {
  as_geos_geometry(wk::as_wkb(x, ...))
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
as_wkb.geos_geometry <- function(x, ..., include_z = TRUE, include_srid = FALSE, endian = 1) {
  # GEOS 3.9 and up can handle the empty point natively
  if (geos_version() >= "3.9.1") {
    out <- unclass(
      geos_write_wkb(
        x,
        include_z = include_z,
        include_srid = include_srid,
        endian = endian
      )
    )

    return(wk::new_wk_wkb(out, crs = attr(x, "crs", exact = TRUE)))
  }

  # otherwise, the GEOS WKB writer errors on empty point, but wk_wkb uses POINT (nan nan)
  is_empty_point <- (geos_type_id(x) == 1L) & geos_is_empty(x) # nocov start

  if (any(is_empty_point, na.rm = TRUE)) {
    out <- rep_len(list(NULL), length(x))
    out[is_empty_point] <- wk::wkt_translate_wkb("POINT EMPTY")
    out[!is_empty_point] <- unclass(
      geos_write_wkb(
        x[!is_empty_point],
        include_z = include_z,
        include_srid = include_srid,
        endian = endian
      )
    )
  } else {
    out <- unclass(
      geos_write_wkb(
        x,
        include_z = include_z,
        include_srid = include_srid,
        endian = endian
      )
    )
  }

  wk::new_wk_wkb(out, crs = attr(x, "crs", exact = TRUE)) # nocov end
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
