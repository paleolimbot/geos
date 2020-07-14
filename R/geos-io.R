
#' Read and write well-known text
#'
#' @param geom A [GEOS geometry vector][as_geos_geometry]
#' @inheritParams wk::wkb_translate_wkt
#' @inheritParams geos_segment_intersection
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
geos_read_xy <- function(point) {
  point <- geos_assert_list_of_numeric(point, 2, "point")
  point <- recycle_common(point)
  new_geos_geometry(.Call(geos_c_read_xy, point[[1]], point[[2]]))
}

#' @rdname geos_read_wkt
#' @export
geos_write_xy <- function(geom) {
  .Call(geos_c_write_xy, as_geos_geometry(geom))
}


#' Create empty geometries
#'
#' @param type_id The numeric type identifier for which an
#'   empty should be returned, an object from which
#'   one can be extracted using [as_geos_type_id()]
#'   (default to calling [geos_type_id()]). This is most
#'   usefully a character vector with the geometry type
#'   (e.g., point, linestring, polygon).
#'
#' @return A [GEOS geometry vector][as_geos_geometry].
#' @export
#'
#' @examples
#' geos_empty(c("point", "linestring", "polygon"))
#' geos_empty(1:7)
#' geos_empty(geos_read_wkt(c("POINT (0 1)", "LINESTRING (0 0, 1 1)")))
#'
geos_empty <- function(type_id = "geometrycollection") {
  new_geos_geometry(.Call(geos_c_empty, as_geos_type_id(type_id)))
}

#' @rdname geos_empty
#' @export
as_geos_type_id <- function(type_id) {
  UseMethod("as_geos_type_id")
}

#' @rdname geos_empty
#' @export
as_geos_type_id.default <- function(type_id) {
  geos_type_id(type_id)
}

#' @rdname geos_empty
#' @export
as_geos_type_id.character <- function(type_id) {
  match(
    tolower(type_id),
    c(
      "point", "linestring", "polygon",
      "multipoint", "multilinestring", "multipolygon",
      "geometrycollection"
    )
  )
}

#' @rdname geos_empty
#' @export
as_geos_type_id.numeric <- function(type_id) {
  as.integer(type_id)
}
