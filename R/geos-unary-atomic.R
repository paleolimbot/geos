
#' Extract information from a GEOS geometry
#'
#' @inheritParams geos_read_wkt
#'
#' @return A vector of length `geom`
#' @export
#'
geos_area <- function(geom) {
  .Call(geos_c_area, as_geos_geometry(geom))
}
