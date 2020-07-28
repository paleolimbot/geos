
#' Read and write well-known text
#'
#' @param geom A [GEOS geometry vector][as_geos_geometry]
#' @inheritParams wk::wkb_translate_wkt
#' @inheritParams geos_segment_intersection
#' @param hex A hexidecimal representation of well-known binary
#'
#' @export
#'
#' @examples
#' geos_read_wkt("POINT (30 10)")
#' geos_write_wkt(geos_read_wkt("POINT (30 10)"))
#'
geos_read_wkt <- function(wkt) {
  new_geos_geometry(.Call(geos_c_read_wkt, as.character(wkt)))
}

#' @rdname geos_read_wkt
#' @export
geos_write_wkt <- function(geom, include_z = TRUE, precision = 16, trim = TRUE) {
  .Call(
    geos_c_write_wkt,
    as_geos_geometry(geom),
    as.logical(include_z),
    as.integer(precision),
    as.logical(trim)
  )
}

#' @rdname geos_read_wkt
#' @export
geos_read_wkb <- function(wkb) {
  new_geos_geometry(.Call(geos_c_read_wkb, as.list(wkb)))
}

#' @rdname geos_read_wkt
#' @export
geos_write_wkb <- function(geom, include_z = TRUE, include_srid = FALSE, endian = 1) {
  structure(
    .Call(
      geos_c_write_wkb,
      as_geos_geometry(geom),
      as.logical(include_z),
      as.logical(include_srid),
      as.integer(endian)
    ),
    class = "blob"
  )
}

#' @rdname geos_read_wkt
#' @export
geos_read_hex <- function(hex) {
  new_geos_geometry(.Call(geos_c_read_hex, as.character(hex)))
}

#' @rdname geos_read_wkt
#' @export
geos_write_hex <- function(geom, include_z = TRUE, include_srid = FALSE, endian = 1) {
  .Call(
    geos_c_write_hex,
    as_geos_geometry(geom),
    as.logical(include_z),
    as.logical(include_srid),
    as.integer(endian)
  )
}

#' @rdname geos_read_wkt
#' @export
geos_read_xy <- function(point) {
  point <- geos_assert_list_of_numeric(point, 2, "point")
  geos_make_point(point[[1]], point[[2]])
}

#' @rdname geos_read_wkt
#' @export
geos_write_xy <- function(geom) {
  .Call(geos_c_write_xy, as_geos_geometry(geom))
}

