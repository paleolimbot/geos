
#' Geometry transformers
#'
#' @inheritParams geos_read_wkt
#' @param tolerance A minimum distance to use for simplification. Use a higher
#'   value for more simplification.
#' @param index The index of the point or geometry to extract.
#' @param rect A `list()` representing rectangles in the form
#'   `list(xmin, ymin, xmax, ymax)`. List items with length 1 will be
#'    recycled to the length of the longest item.
#' @param srid An integer spatial reference identifier.
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
#' geos_minimum_bounding_circle("LINESTRING (-1 -1, 1 1)")
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
#' geos_point_start("LINESTRING (0 0, 1 1)")
#' geos_point_end("LINESTRING (0 0, 1 1)")
#'
#' geos_interpolate("LINESTRING (0 0, 1 1)", 1)
#' geos_interpolate_normalized("LINESTRING (0 0, 1 1)", 1)
#' geos_simplify("LINESTRING (0 0, 0 1, 0 2)", 0.1)
#' geos_simplify_preserve_topology("LINESTRING (0 0, 0 1, 0 2)", 0.1)
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
geos_minimum_bounding_circle <- function(geom) {
  new_geos_geometry(.Call(geos_c_minimum_bounding_circle, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_unary_union <- function(geom) {
  new_geos_geometry(.Call(geos_c_unary_union, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_coverage_union <- function(geom) {
  new_geos_geometry(.Call(geos_c_coverage_union, as_geos_geometry(geom)))
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
geos_point_start <- function(geom) {
  new_geos_geometry(.Call(geos_c_point_start, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_point_end <- function(geom) {
  new_geos_geometry(.Call(geos_c_point_end, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_clone <- function(geom) {
  new_geos_geometry(.Call(geos_c_clone, as_geos_geometry(geom)))
}

#' @rdname geos_centroid
#' @export
geos_set_srid <- function(geom, srid) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.integer(srid)))
  new_geos_geometry(.Call(geos_c_set_srid, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_centroid
#' @export
geos_point_n <- function(geom, index) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.integer(index) - 1L))
  new_geos_geometry(.Call(geos_c_point_n, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_centroid
#' @export
geos_simplify <- function(geom, tolerance) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.numeric(tolerance)))
  new_geos_geometry(.Call(geos_c_simplify, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_centroid
#' @export
geos_simplify_preserve_topology <- function(geom, tolerance) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.numeric(tolerance)))
  new_geos_geometry(.Call(geos_c_simplify_preserve_topology, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_centroid
#' @export
geos_clip_by_rect <- function(geom, rect) {
  rect <- geos_assert_list_of_numeric(rect, 4, "rect")
  recycled <- recycle_common(c(list(as_geos_geometry(geom)), rect))
  new_geos_geometry(
    .Call(
      geos_c_clip_by_rect,
      recycled[[1]],
      recycled[[2]], recycled[[3]], recycled[[4]], recycled[[5]]
    )
  )
}

# --- these are documented with geos_project ---

#' @rdname geos_project
#' @export
geos_interpolate <- function(geom, distance) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.numeric(distance)))
  new_geos_geometry(.Call(geos_c_interpolate, recycled[[1]], recycled[[2]]))
}

#' @rdname geos_project
#' @export
geos_interpolate_normalized <- function(geom, distance_normalized) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.numeric(distance_normalized)))
  new_geos_geometry(.Call(geos_c_interpolate_normalized, recycled[[1]], recycled[[2]]))
}


#' Buffer a geometry
#'
#' - [geos_buffer()] returns a polygon or multipolygon geometry.
#' - [geos_offset_curve()] returns a linestring offset to the left by `distance`.
#'
#' @inheritParams geos_read_wkt
#' @param params A [geos_buffer_params()]
#' @param distance The buffer distance. Can be negative to buffer
#'   or offset on the righthand side of the geometry.
#' @param quad_segs The number of segments per quadrant. A higher number
#'   here will increase the apparent resolution of the resulting polygon.
#' @param end_cap_style One of "round", "flat", or "square".
#' @param join_style One of "round", "mitre", or "bevel".
#' @param mitre_limit If `join_style` is "mitre", the relative extent (from zero to one)
#'   of the join.
#' @param single_sided Use `TRUE` to buffer on only the right side
#'   of the geometry. This does not apply to [geos_offset_curve()], which is always
#'   one-sided.
#'
#' @return A [GEOS geometry vector][as_geos_geometry] along the recycled
#'   length of `geom` and `distance`.
#'
#' @export
#'
#' @examples
#' geos_buffer("POINT (0 0)", 1)
#' geos_offset_curve("LINESTRING (0 0, 0 10, 10 0)", 1)
#'
geos_buffer <- function(geom, distance, params = geos_buffer_params()) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.numeric(distance)))

  result <- .Call(
    geos_c_buffer,
    recycled[[1]],
    recycled[[2]],
    params
  )

  new_geos_geometry(result)
}

#' @rdname geos_buffer
#' @export
geos_offset_curve <- function(geom, distance, params = geos_buffer_params()) {
  recycled <- recycle_common(list(as_geos_geometry(geom), as.numeric(distance)))

  result <- .Call(
    geos_c_offset_curve,
    recycled[[1]],
    recycled[[2]],
    params
  )

  new_geos_geometry(result)
}

#' @rdname geos_buffer
#' @export
geos_buffer_params <- function(quad_segs = 30,
                               end_cap_style = c("round", "flat", "square"),
                               join_style = c("round", "mitre", "bevel"),
                               mitre_limit = 1,
                               single_sided = FALSE) {
  end_cap_style <- match.arg(end_cap_style)
  join_style <- match.arg(join_style)

  structure(
    list(
      quad_segs = as.integer(quad_segs),
      end_cap_style = match(end_cap_style, c("round", "flat", "square")),
      join_style = match(join_style, c("round", "mitre", "bevel")),
      mitre_limit = as.numeric(mitre_limit),
      single_sided = as.logical(single_sided)
    ),
    class = "geos_buffer_params"
  )
}


#' Delaunay triagulations and Voronoi diagrams
#'
#' These functions return one triangulation/diagram per feature as a
#' multi geometry. These functions are not vectorized along their parameters.
#'
#' @param geom A [GEOS geometry vector][as_geos_geometry] whose nodes will be used
#'   as input.
#' @param tolerance A snapping tolerance or 0 to disable snapping
#' @param env A boundary for the diagram, or `NULL` to construct one
#'   based on the input
#'
#' @return A [GEOS geometry vector][as_geos_geometry] of length `geom`
#' @export
#'
#' @examples
#' geos_delaunay_triangles("MULTIPOINT (0 0, 1 0, 0 1)")
#' geos_delaunay_edges("MULTIPOINT (0 0, 1 0, 0 1)")
#'
#' geos_voronoi_polygons("MULTIPOINT (0 0, 1 0, 0 1)")
#' geos_voronoi_edges("MULTIPOINT (0 0, 1 0, 0 1)")
#'
geos_delaunay_triangles <- function(geom, tolerance = 0) {
  new_geos_geometry(.Call(geos_c_delaunay_triangulation, as_geos_geometry(geom), tolerance, FALSE))
}

#' @rdname geos_delaunay_triangles
#' @export
geos_delaunay_edges <- function(geom, tolerance = 0) {
  new_geos_geometry(.Call(geos_c_delaunay_triangulation, as_geos_geometry(geom), tolerance, TRUE))
}

#' @rdname geos_delaunay_triangles
#' @export
geos_voronoi_polygons <- function(geom, env = NULL, tolerance = 0) {
  env <- get_env_xptr(env)
  new_geos_geometry(.Call(geos_c_voronoi_diagram, as_geos_geometry(geom), env, tolerance, FALSE))
}

#' @rdname geos_delaunay_triangles
#' @export
geos_voronoi_edges <- function(geom, env = NULL, tolerance = 0) {
  env <- get_env_xptr(env)
  new_geos_geometry(.Call(geos_c_voronoi_diagram, as_geos_geometry(geom), env, tolerance, TRUE))
}

get_env_xptr <- function(env) {
  if (!is.null(env)) {
    env <- as_geos_geometry(env)
    stopifnot(length(env) == 1)
    unclass(env)[[1]]
  } else {
    env
  }
}
