
#' Geometry transformers
#'
#' @inheritParams geos_read_wkt
#' @param tolerance A minimum distance to use for simplification or
#'   densification. Use a higher value for more simplification (or
#'   less densification).
#' @param index The index of the point or geometry to extract.
#' @param rect A `list()` representing rectangles in the form
#'   `list(xmin, ymin, xmax, ymax)`. List items with length 1 will be
#'   recycled to the length of the longest item.
#' @param srid An integer spatial reference identifier.
#' @param grid_size The size of the grid to which coordinates should be
#'   rounded.
#' @param ratio The ratio between the area of the concave hull and the area
#'   of the return value. Use 1 for the concave hull; use 0 for maximum
#'   concave-ness.
#' @param ratio_mode One of "vertex" or "area", describing the normalized
#'   proportion type for which `ratio` represents.
#' @param hull_type One of "outer" or "inner".
#' @param allow_holes Use `TRUE` to allow the concave hull to contain holes
#' @param is_tight Use `FALSE` to allow concave hull to expand beyond the
#'   convex hull.
#' @param preserve_topology Should topology internal to each feature
#'   be preserved?
#' @param keep_collapsed Should items that become EMPTY due to rounding
#'   be kept in the output?
#' @param make_valid_params A [geos_make_valid_params()] object.
#' @param method The method to use for [geos_make_valid()].  One of:
#'   - "make_valid_linework" combines all rings into a set of noded lines
#'     and then extracts valid polygons from that linework.
#'   - "make_valid_structure" Structured method, first makes all rings valid
#'     then merges shells and subtracts holes from shells to generate valid
#'     result. Assumes that holes and shells are correctly categorized.
#' @param grid_size For `_prec()` variants, the grid size such that all vertices of
#'   the resulting geometry will lie on the grid.
#' @param trans A [wk transform][wk::as_wk_trans] object.
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
#' geos_simplify("LINESTRING (0 0, 0 1, 0 2)", 0.1)
#' geos_simplify_preserve_topology("LINESTRING (0 0, 0 1, 0 2)", 0.1)
#'
geos_centroid <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_centroid, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_boundary <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_boundary, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_minimum_width <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_minimum_width, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_minimum_clearance_line <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_minimum_clearance_line, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_minimum_rotated_rectangle <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_minimum_rotated_rectagle, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' Circular approximations
#'
#' @inheritParams geos_centroid
#' @param tolerance Threshold for considering circles to be touching
#'   a boundary.
#' @param boundary An outer boundary for the largest empty circle
#'   algorithm.
#'
#' @export
geos_minimum_bounding_circle <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  result <- .Call(geos_c_minimum_bounding_circle, geom)
  attributes(result) <- NULL
  new_geos_geometry(result, crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_minimum_bounding_circle
#' @export
geos_minimum_bounding_crc <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  result <- .Call(geos_c_minimum_bounding_circle, geom)
  wk::crc(
    x = attr(result, "x"),
    y = attr(result, "y"),
    r = attr(result, "radius"),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_minimum_bounding_circle
#' @export
geos_maximum_inscribed_circle_spec <- function(geom, tolerance) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(tolerance)))
  new_geos_geometry(
    .Call(geos_c_maximum_inscribed_circle, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_minimum_bounding_circle
#' @export
geos_maximum_inscribed_crc <- function(geom, tolerance) {
  spec <- geos_maximum_inscribed_circle_spec(geom, tolerance)
  xy <- unclass(as_xy(geos_point_start(spec)))

  wk::crc(
    xy$x, xy$y,
    geos_length(spec),
    crs = attr(spec, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_unary_union <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_unary_union, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_unary_union_prec <- function(geom, grid_size) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(grid_size)))
  new_geos_geometry(
    .Call(geos_c_unary_union_prec, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_coverage_union <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_coverage_union, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_point_on_surface <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_point_on_surface, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_node <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_node, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_make_valid <- function(geom, make_valid_params = geos_make_valid_params()) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(
    .Call(geos_c_make_valid_with_params, geom, make_valid_params),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_make_valid_params <- function(keep_collapsed = TRUE,
                                   method = c("make_valid_linework", "make_valid_structure")) {
  method <- match.arg(method)

  structure(
    list(
      keep_collapsed = sanitize_logical_scalar(keep_collapsed),
      method = match(method, c("make_valid_linework", "make_valid_structure")) - 1L
    ),
    class = "geos_make_valid_params"
  )
}

#' @rdname geos_centroid
#' @export
geos_unique_points <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_unique_points, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_reverse <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_reverse, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_merge_lines <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_merge_lines, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_build_area <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_build_area, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_envelope <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_envelope, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_envelope_rct <- function(geom) {
  geom <- sanitize_geos_geometry(geom)

  # geos_xmin() and friends errors on EMPTY
  not_empty <- is.na(geom) | !geos_is_empty(geom)

  result <- list(
    xmin = rep(Inf, length(geom)),
    ymin = rep(Inf, length(geom)),
    xmax = rep(-Inf, length(geom)),
    ymax = rep(-Inf, length(geom))
  )

  result$xmin[not_empty] <- geos_xmin(geom[not_empty])
  result$ymin[not_empty] <- geos_ymin(geom[not_empty])
  result$xmax[not_empty] <- geos_xmax(geom[not_empty])
  result$ymax[not_empty] <- geos_ymax(geom[not_empty])

  wk::new_wk_rct(result, crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_extent <- function(geom) {
  if (geos_version() >= "3.11.0") {
    geom <- sanitize_geos_geometry(geom)
    new_data_frame(.Call(geos_c_extent, geom))
  } else {
    as.data.frame(geos_envelope_rct(geom))
  }
}

#' @rdname geos_centroid
#' @export
geos_convex_hull <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_convex_hull, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_concave_hull <- function(geom, ratio, allow_holes = FALSE) {
  recycled <- recycle_common(list(sanitize_geos_geometry(geom), ratio))
  new_geos_geometry(
    .Call(geos_c_concave_hull, recycled[[1]], recycled[[2]], as.integer(allow_holes)[1]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_concave_hull_of_polygons <- function(geom, ratio, is_tight = TRUE, allow_holes = FALSE) {
  recycled <- recycle_common(list(sanitize_geos_geometry(geom), ratio))
  new_geos_geometry(
    .Call(
      geos_c_concave_hull_of_polygons,
      recycled[[1]],
      recycled[[2]],
      as.integer(is_tight)[1],
      as.integer(allow_holes)[1]
    ),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_polygon_hull_simplify <- function(geom, ratio,
                                       hull_type = c("outer", "inner"),
                                       ratio_mode = c("vertex", "area")) {
  hull_type <- match.arg(hull_type)
  is_outer <- identical(hull_type, "outer")

  ratio_mode <- match.arg(ratio_mode)
  ratio_mode_int <- match(ratio_mode, c("vertex", "area"))

  recycled <- recycle_common(list(sanitize_geos_geometry(geom), ratio))
  new_geos_geometry(
    .Call(
      geos_c_polygon_hull_simplify,
      recycled[[1]],
      recycled[[2]],
      is_outer,
      ratio_mode_int
    ),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_point_start <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_point_start, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_point_end <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_point_end, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_line_merge <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_line_merge, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_line_merge_directed <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_line_merge_directed, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_transform_xy <- function(geom, trans) {
  geom <- sanitize_geos_geometry(geom)
  trans <- wk::as_wk_trans(trans)
  new_geos_geometry(.Call(geos_c_transform_xy, geom, trans), crs = NULL)
}

#' @rdname geos_centroid
#' @export
geos_clone <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_clone, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_set_srid <- function(geom, srid) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_integer(srid)))
  new_geos_geometry(
    .Call(geos_c_set_srid, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_point_n <- function(geom, index) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_integer(index) - 1L))
  new_geos_geometry(
    .Call(geos_c_point_n, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_simplify <- function(geom, tolerance) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(tolerance)))
  new_geos_geometry(
    .Call(geos_c_simplify, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_remove_repeated_points <- function(geom, tolerance) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(tolerance)))
  new_geos_geometry(
    .Call(geos_c_remove_repeated_points, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_simplify_preserve_topology <- function(geom, tolerance) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(tolerance)))
  new_geos_geometry(
    .Call(geos_c_simplify_preserve_topology, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_set_precision <- function(geom, grid_size, preserve_topology = TRUE, keep_collapsed = FALSE) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(grid_size)))
  new_geos_geometry(
    .Call(
      geos_c_set_precision,
      recycled[[1]],
      recycled[[2]],
      preserve_topology,
      keep_collapsed
    ),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_centroid
#' @export
geos_normalize <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(.Call(geos_c_normalize, geom), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_densify <- function(geom, tolerance) {
  recycled <- recycle_common(list(sanitize_geos_geometry(geom), as.numeric(tolerance)))
  new_geos_geometry(.Call(geos_c_densify, recycled[[1]], recycled[[2]]), crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_centroid
#' @export
geos_clip_by_rect <- function(geom, rect) {
  geom <- sanitize_geos_geometry(geom)
  if (inherits(rect, "wk_rct")) {
    wk_crs_output(geom, rect)
    rect <- unclass(rect)
  } else {
    rect <- geos_assert_list_of_numeric(rect, 4, "rect")
  }
  recycled <- recycle_common(c(list(geom), rect))
  new_geos_geometry(
    .Call(
      geos_c_clip_by_rect,
      recycled[[1]],
      recycled[[2]], recycled[[3]], recycled[[4]], recycled[[5]]
    ),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

# --- these are documented with geos_project ---

#' @rdname geos_project
#' @export
geos_interpolate <- function(geom, distance) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(distance)))
  new_geos_geometry(
    .Call(geos_c_interpolate, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_project
#' @export
geos_interpolate_normalized <- function(geom, distance_normalized) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(distance_normalized)))
  new_geos_geometry(
    .Call(geos_c_interpolate_normalized, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
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
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(distance)))

  result <- .Call(
    geos_c_buffer,
    recycled[[1]],
    recycled[[2]],
    params
  )

  new_geos_geometry(result, crs = attr(geom, "crs", exact = TRUE))
}

#' @rdname geos_buffer
#' @export
geos_offset_curve <- function(geom, distance, params = geos_buffer_params()) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_double(distance)))

  result <- .Call(
    geos_c_offset_curve,
    recycled[[1]],
    recycled[[2]],
    params
  )

  new_geos_geometry(result, crs = attr(geom, "crs", exact = TRUE))
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
      quad_segs = sanitize_integer_scalar(quad_segs),
      end_cap_style = match(end_cap_style, c("round", "flat", "square")),
      join_style = match(join_style, c("round", "mitre", "bevel")),
      mitre_limit = sanitize_double_scalar(mitre_limit),
      single_sided = sanitize_logical_scalar(single_sided)
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
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(
    .Call(geos_c_delaunay_triangulation, geom, tolerance, FALSE),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_delaunay_triangles
#' @export
geos_constrained_delaunay_triangles <- function(geom) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(
    .Call(geos_c_constrained_delaunay_triangulation, geom),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_delaunay_triangles
#' @export
geos_delaunay_edges <- function(geom, tolerance = 0) {
  geom <- sanitize_geos_geometry(geom)
  new_geos_geometry(
    .Call(geos_c_delaunay_triangulation, geom, tolerance, TRUE),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_delaunay_triangles
#' @export
geos_voronoi_polygons <- function(geom, env = NULL, tolerance = 0) {
  geom <- sanitize_geos_geometry(geom)
  env <- get_env_xptr(env)
  new_geos_geometry(
    .Call(geos_c_voronoi_diagram, geom, env, tolerance, FALSE),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_delaunay_triangles
#' @export
geos_voronoi_edges <- function(geom, env = NULL, tolerance = 0) {
  geom <- sanitize_geos_geometry(geom)
  env <- get_env_xptr(env)
  new_geos_geometry(
    .Call(geos_c_voronoi_diagram, geom, env, tolerance, TRUE),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

get_env_xptr <- function(env) {
  if (!is.null(env)) {
    env <- sanitize_geos_geometry(env)
    stopifnot(length(env) == 1)
    unclass(env)[[1]]
  } else {
    env
  }
}

#' Access child geometries
#'
#' @inheritParams geos_read_wkt
#' @param n The (one-based) index of the child geometry
#'
#' @export
#' @return A [GEOS geometry vector][as_geos_geometry] along the recycled
#'   length of `geom` and `i`.
#'
#' @examples
#' multipoint <- "MULTIPOINT (0 0, 1 1, 2 2)"
#' geos_geometry_n(multipoint, seq_len(geos_num_geometries(multipoint)))
#'
#' poly <- "POLYGON ((0 0, 0 1, 1 0, 0 0), (0.1 0.1, 0.1 0.2, 0.2 0.1, 0.1 0.1))"
#' geos_ring_n(poly, seq_len(geos_num_rings(poly)))
#'
geos_geometry_n <- function(geom, n) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_integer(n) - 1L))
  new_geos_geometry(
    .Call(geos_c_geometry_n, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_geometry_n
#' @export
geos_ring_n <- function(geom, n) {
  geom <- sanitize_geos_geometry(geom)
  recycled <- recycle_common(list(geom, sanitize_integer(n) - 1L))
  new_geos_geometry(
    .Call(geos_c_ring_n, recycled[[1]], recycled[[2]]),
    crs = attr(geom, "crs", exact = TRUE)
  )
}
