vect.geos_geometry <- function(x) {
  terra::vect(
    geos::geos_write_wkt(x),
    crs = wk::wk_crs(x)$wkt
  )
}
