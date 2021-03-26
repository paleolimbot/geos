
vec_proxy.geos_geometry <- function(x, ...) {
  unclass(x)
}

vec_restore.geos_geometry <- function(x, ...) {
  new_geos_geometry(x)
}

vec_ptype_abbr.geos_geometry <- function(x, ...) {
  "geos_geom"
}
