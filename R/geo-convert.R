
#' Convert a geometry to a different data structure
#'
#' @inheritParams geo_ptype
#'
#' @return A geometry vector, in the format defined by `to`.
#' @export
#'
#' @examples
#' geo_convert(geo_wkt("POINT (20 10)"), geo_wkb())
#' geo_convert(geo_wkt("POINT (20 10)"), geo_coord())
#'
geo_convert <- function(x, to) {
  geo_restore(to, cpp_convert(x, to))
}
