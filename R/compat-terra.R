# S4 method for converting geos_geometry to SpatVector
vect_geos_geometry <- function(x) {
  geos_crs <- wk::wk_crs(x)

  if (is.list(geos_crs)) {
    crs <- geos_crs$wkt
  } else {
    crs <- as.character(geos_crs)
  }

  terra::vect(
    geos::geos_write_wkt(x),
    crs = crs
  )
}
