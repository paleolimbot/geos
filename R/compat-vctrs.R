
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
