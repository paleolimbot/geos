
#' @rdname as_geos_geometry
#' @export
as_geos_geometry.sfc <- function(x, ...) {
  geos <- wk::wk_handle(x, geos_geometry_writer())
  attr(geos, "crs") <- sf::st_crs(x)
  geos
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.sf <- function(x, ...) {
  geos <- wk::wk_handle(x, geos_geometry_writer())
  attr(geos, "crs") <- sf::st_crs(x)
  geos
}

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
