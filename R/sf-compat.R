
#' @rdname as_geos_geometry
#' @export
as_geos_geometry.sfc <- function(x, ...) {
  geos_read_wkb(sf::st_as_binary(x, EWKB = FALSE))
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.sf <- function(x, ...) {
  geos_read_wkb(sf::st_as_binary(sf::st_geometry(x), EWKB = FALSE))
}

# dynamically exported
st_as_sfc.geos_geometry <- function(x, ...) {
  # use wk's exporter to handle the empty point
  wk_wkb <- as_wkb.geos_geometry(x)
  sf::st_as_sfc(structure(wk_wkb, class = "WKB"))
}

st_as_sf.geos_geometry <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.geos_geometry(x))
    )
  )
}
