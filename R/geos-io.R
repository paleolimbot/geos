
#' Read and write well-known text
#'
#' @param geom A [GEOS geometry vector][as_geos_geometry]
#' @inheritParams wk::wkb_translate_wkt
#'
#' @export
#'
#' @examples
#' geos_read_wkt("POINT (30 10)")
#' geos_write_wkt(as_geos_geometry("POINT (30 10)"))
#'
geos_read_wkt <- function(wkt) {
  new_geos_geometry(.Call(geos_c_read_wkt, wkt))
}

#' @rdname geos_read_wkt
#' @export
geos_write_wkt <- function(geom) {
  .Call(geos_c_write_wkt, geom)
}

#' @rdname geos_read_wkt
#' @export
geos_read_wkb <- function(wkb) {
  new_geos_geometry(.Call(geos_c_read_wkb, wkb))
}

#' @rdname geos_read_wkt
#' @export
geos_write_wkb <- function(geom) {
  structure(.Call(geos_c_write_wkb, geom), class = "blob")
}
