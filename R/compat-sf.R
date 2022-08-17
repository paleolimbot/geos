
# dynamically exported
st_as_sfc.geos_geometry <- function(x, ...) {
  sfc <- wk::wk_handle(x, wk::sfc_writer())
  sf::st_crs(sfc) <- sf::st_crs(attr(x, "crs", exact = TRUE))
  sfc
}

st_as_sf.geos_geometry <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.geos_geometry(x))
    )
  )
}
