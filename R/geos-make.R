
#' Create geometries from vectors of coordinates
#'
#' @inheritParams wkutils::coords_point_translate_wkt
#' @inheritParams geos_empty
#' @inheritParams geos_write_wkt
#'
#' @return A [GEOS geometry vector][as_geos_geometry]
#' @export
#'
#' @examples
#' geos_make_point(1:3, 1:3)
#' geos_make_linestring(1:3, 1:3)
#'
#' geos_make_collection("POINT (1 1)")
#'
geos_make_point <- function(x, y, z = NA_real_) {
  point <- recycle_common(list(as.numeric(x), as.numeric(y), as.numeric(z)))
  new_geos_geometry(.Call(geos_c_make_point, point[[1]], point[[2]], point[[3]]))
}

#' @rdname geos_make_point
#' @export
geos_make_linestring <- function(x, y, z = NA_real_, feature_id = 1L) {
  recycled <- recycle_common(
    list(
      as.numeric(x), as.numeric(y), as.numeric(z),
      as.integer(feature_id)
    )
  )
  lengths <- rle(recycled[[4]])$lengths

  if (length(lengths) == 0) {
    geos_empty("linestring")
  } else {
    new_geos_geometry(
      .Call(
        geos_c_make_linestring,
        recycled[[1]], recycled[[2]], recycled[[3]],
        lengths
      )
    )
  }
}

#' @rdname geos_make_point
#' @export
geos_make_polygon <- function(x, y, z = NA_real_, feature_id = 1L, ring_id = 1L) {
  # nocov start
  recycled <- recycle_common(
    list(
      as.numeric(x), as.numeric(y), as.numeric(z),
      as.integer(feature_id), as.integer(ring_id)
    )
  )
  stop("not implemented")
  # nocov end
}

#' @rdname geos_make_point
#' @export
geos_make_collection <- function(geom, type_id = "geometrycollection", feature_id = 1L) {
  type_id <- as_geos_type_id(type_id)
  stopifnot(length(type_id) == 1)

  recycled <- recycle_common(list(as_geos_geometry(geom), as.integer(feature_id)))
  lengths <- rle(recycled[[2]])$lengths

  # it's unlikely that anybody wants zero-length output, which is
  # otherwise what would happen if length(geom) == 0
  if (length(lengths) == 0) {
    geos_empty(type_id)
  } else {
    new_geos_geometry(.Call(geos_c_make_collection, recycled[[1]], type_id, lengths))
  }
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
