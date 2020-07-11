
#' Geometry transformers
#'
#' @inheritParams geos_read_wkt
#'
#' @return A [GEOS geometry vector][as_geos_geometry] of length `geom`
#' @export
#'
#' @examples
#' geos_centroid(c("POINT (0 1)", "LINESTRING (0 0, 1 1)"))
#' geos_boundary(c("POLYGON ((0 0, 1 0, 0 1, 0 0))", "LINESTRING (0 0, 1 1)"))
#' geos_minimum_width("POLYGON ((0 0, 1 0, 0 1, 0 0))")
#' geos_minimum_clearance_line("POLYGON ((0 0, 10 0, 10 10, 3 5, 0 10, 0 0))")
#' geos_minimum_rotated_rectangle("POLYGON ((0 0, 1 0, 0.5 0.5, 0 0))")
#' geos_unary_union("MULTIPOINT (0 1, 0 1)")
#' geos_point_on_surface("LINESTRING (0 1, 0.2 3, 10 10)")
#' geos_node("POLYGON ((0 0, 1 0, 0 1, 0 0))")
#' geos_make_valid("POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))")
#' geos_unique_points("POLYGON ((0 0, 1 0, 0 1, 0 0))")
#' geos_reverse("LINESTRING (0 0, 1 1)")
#' geos_merge_lines(
#'   "MULTILINESTRING ((0 0, 0.5 0.5, 2 2), (0.5 0.5, 2 2))"
#' )
#' geos_build_area("LINESTRING (0 0, 1 0, 0 1, 0 0)")
#' geos_envelope("LINESTRING (0 0, 1 2)")
#' geos_convex_hull("MULTIPOINT (0 0, 1 0, 0 2, 0 0)")
#'
geos_centroid <- function(geom) {
  new_geos_geometry(.Call(geos_c_centroid, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_boundary <- function(geom) {
  new_geos_geometry(.Call(geos_c_boundary, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_minimum_width <- function(geom) {
  new_geos_geometry(.Call(geos_c_minimum_width, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_minimum_clearance_line <- function(geom) {
  new_geos_geometry(.Call(geos_c_minimum_clearance_line, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_minimum_rotated_rectangle <- function(geom) {
  new_geos_geometry(.Call(geos_c_minimum_rotated_rectagle, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_unary_union <- function(geom) {
  new_geos_geometry(.Call(geos_c_unary_union, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_point_on_surface <- function(geom) {
  new_geos_geometry(.Call(geos_c_point_on_surface, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_node <- function(geom) {
  new_geos_geometry(.Call(geos_c_node, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_make_valid <- function(geom) {
  new_geos_geometry(.Call(geos_c_make_valid, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_unique_points <- function(geom) {
  new_geos_geometry(.Call(geos_c_unique_points, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_reverse <- function(geom) {
  new_geos_geometry(.Call(geos_c_reverse, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_merge_lines <- function(geom) {
  new_geos_geometry(.Call(geos_c_merge_lines, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_build_area <- function(geom) {
  new_geos_geometry(.Call(geos_c_build_area, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_envelope <- function(geom) {
  new_geos_geometry(.Call(geos_c_envelope, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_convex_hull <- function(geom) {
  new_geos_geometry(.Call(geos_c_convex_hull, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_clone <- function(geom) {
  new_geos_geometry(.Call(geos_c_clone, as_geos_geometry(geom)))
}
