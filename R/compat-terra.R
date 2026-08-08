# S4 method for converting geos_geometry to SpatVector
vect_geos_geometry <- function(x) {
  geos_crs <- wk::wk_crs(x)

  if (is.null(geos_crs)) {
    crs <- ""
  } else if (is.list(geos_crs)) {
    crs <- geos_crs$wkt
  } else {
    crs <- as.character(geos_crs)
  }

  terra::vect(
    ## needs 'raw' in a list, not 'blob'
    unclass(geos::geos_write_wkb(x)),
    crs = crs
  )
}
