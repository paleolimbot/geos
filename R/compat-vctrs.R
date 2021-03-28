
#' Vctrs methods
#'
#' @param x,y,to,... See [vctrs::vec_cast()] and [vctrs::vec_ptype2()].
#' @rdname vctrs-methods
#' @name vctrs-methods
#'
NULL

vec_proxy.geos_geometry <- function(x, ...) {
  unclass(x)
}

vec_restore.geos_geometry <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  new_geos_geometry(x, crs = crs_out)
}

vec_ptype_abbr.geos_geometry <- function(x, ...) {
  "geos_geom"
}

# Cast *to* geos_geometry -----------

#' @rdname vctrs-methods
#' @export vec_cast.geos_geometry
vec_cast.geos_geometry <- function(x, to, ...) {
  UseMethod("vec_cast.geos_geometry") # nocov
}

#' @method vec_cast.geos_geometry default
#' @export
vec_cast.geos_geometry.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.geos_geometry geos_geometry
#' @export
vec_cast.geos_geometry.geos_geometry <- function(x, to, ...) {
  wk_crs_output(x, to)
  x
}

#' @method vec_cast.geos_geometry wk_wkt
#' @export
vec_cast.geos_geometry.wk_wkt <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_geos_geometry(x)
}

#' @method vec_cast.geos_geometry wk_wkb
#' @export
vec_cast.geos_geometry.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_geos_geometry(x)
}

#' @method vec_cast.geos_geometry wk_xy
#' @export
vec_cast.geos_geometry.wk_xy <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_geos_geometry(x)
}

#' @method vec_cast.geos_geometry wk_xyz
#' @export
vec_cast.geos_geometry.wk_xyz <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_geos_geometry(x)
}

#' @method vec_cast.geos_geometry wk_rct
#' @export
vec_cast.geos_geometry.wk_rct <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_geos_geometry(x)
}

#' @method vec_cast.geos_geometry wk_crc
#' @export
vec_cast.geos_geometry.wk_crc <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_geos_geometry(x)
}

# Cast *from* geos_geometry -----------

#' @importFrom wk vec_cast.wk_wkt
#' @method vec_cast.wk_wkt geos_geometry
#' @export
vec_cast.wk_wkt.geos_geometry <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk::as_wkt(x)
}

#' @importFrom wk vec_cast.wk_wkb
#' @method vec_cast.wk_wkb geos_geometry
#' @export
vec_cast.wk_wkb.geos_geometry <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk::as_wkb(x)
}

#' @importFrom wk vec_cast.wk_xy
#' @method vec_cast.wk_xy geos_geometry
#' @export
vec_cast.wk_xy.geos_geometry <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk::as_xy(x)
}

#' @importFrom wk vec_cast.wk_xyz
#' @method vec_cast.wk_xyz geos_geometry
#' @export
vec_cast.wk_xyz.geos_geometry <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk::as_xy(x, dims = c("x", "y", "z"))
}

# ptype2 ------------------------------

#' @rdname vctrs-methods
#' @export vec_ptype2.geos_geometry
vec_ptype2.geos_geometry <- function(x, y, ...) {
  UseMethod("vec_ptype2.geos_geometry", y) # nocov
}

#' @method vec_ptype2.geos_geometry default
#' @export
vec_ptype2.geos_geometry.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.geos_geometry geos_geometry
#' @export
vec_ptype2.geos_geometry.geos_geometry <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_geos_geometry(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.geos_geometry wk_wkt
#' @export
vec_ptype2.geos_geometry.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_geos_geometry(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.geos_geometry wk_wkb
#' @export
vec_ptype2.geos_geometry.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_geos_geometry(crs = wk_crs_output(x, y))
}


#' @method vec_ptype2.geos_geometry wk_xy
#' @export
vec_ptype2.geos_geometry.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_geos_geometry(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.geos_geometry wk_xyz
#' @export
vec_ptype2.geos_geometry.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_geos_geometry(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.geos_geometry wk_rct
#' @export
vec_ptype2.geos_geometry.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_geos_geometry(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.geos_geometry wk_crc
#' @export
vec_ptype2.geos_geometry.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_geos_geometry(crs = wk_crs_output(x, y))
}
