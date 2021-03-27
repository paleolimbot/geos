
#' @rdname as_geos_geometry
#' @export
as_geos_geometry.sfc <- function(x, ...) {
  geos_read_wkb(sf::st_as_binary(x, EWKB = FALSE), crs = sf::st_crs(x))
}

#' @rdname as_geos_geometry
#' @export
as_geos_geometry.sf <- function(x, ...) {
  geos_read_wkb(sf::st_as_binary(sf::st_geometry(x), EWKB = FALSE),  crs = sf::st_crs(x))
}

# dynamically exported
st_as_sfc.geos_geometry <- function(x, ...) {
  if (geos_version() >= "3.9.1") {
    wk_wkb <- geos_write_wkb(x)
  } else {
    # use wk's exporter to handle the empty point for older GEOS
    wk_wkb <- as_wkb.geos_geometry(x) # nocov
  }

  sfc <- sf::st_as_sfc(structure(wk_wkb, class = "WKB"))
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
