
#' Geometry transformers
#'
#' @inheritParams geos_read_wkt
#'
#' @return A [GEOS geometry vector][as_geos_geometry] of length `geom`
#' @export
#'
geos_centroid <- function(geom) {
  new_geos_geometry(.Call(geos_c_centroid, as_geos_geometry(geom)))
}
