
#' Distance calculations
#'
#' @param geom1,geom2 [GEOS geometry vectors][as_geos_geometry],
#'  recycled to a common length.
#' @param densify A fraction between 0 and 1 denoting the degree to which
#'  edges should be subdivided (smaller value means more subdivisions).
#'  Use NULL to calculate the distance as-is.
#'
#' @return A numeric vector along the recycled length of `geom1` and `geom2`
#' @export
#'
geos_distance <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_distance, recycled[[1]], recycled[[2]])
}

#' @rdname geos_distance
#' @export
geos_distance_indexed <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_distance_indexed, recycled[[1]], recycled[[2]])
}

#' @rdname geos_distance
#' @export
geos_distance_hausdorff <- function(geom1, geom2, densify = NULL) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))

  if (is.null(densify)) {
    .Call(geos_c_distance_hausdorff, recycled[[1]], recycled[[2]])
  } else {
    densify <- as.numeric(densify)
    .Call(geos_c_distance_hausdorff_densify, recycled[[1]], recycled[[2]], densify[1])
  }
}

#' @rdname geos_distance
#' @export
geos_distance_frechet <- function(geom1, geom2, densify = NULL) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))

  if (is.null(densify)) {
    .Call(geos_c_distance_frechet, recycled[[1]], recycled[[2]])
  } else {
    desnify <- as.numeric(densify)
    .Call(geos_c_distance_frechet_densify, recycled[[1]], recycled[[2]], densify[1])
  }
}

#' Linear referencing
#'
#' - [geos_project()] and [geos_project_normalized()] return
#'   the distance of point `geom2` projected on `geom1` from the origin
#'   of `geom1`, which must be a lineal geometry.
#' - [geos_interpolate()] performs an
#'   inverse operation, returning the point along `geom` representing
#'   the given `distance` from the origin along the geometry.
#' - `_normalized()` variants use a distance normalized to the
#'   [geos_length()] of the geometry.
#'
#' @inheritParams geos_distance
#' @inheritParams geos_read_wkt
#' @param distance Distance along the linestring to interpolate
#' @param distance_normalized Distance along the linestring to interpolate
#'   relative to the length of the linestring.
#'
#' @export
#'
geos_project <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_project, recycled[[1]], recycled[[2]])
}

#' @rdname geos_project
#' @export
geos_project_normalized <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_project_normalized, recycled[[1]], recycled[[2]])
}

#' Binary predicates
#'
#' @inheritParams geos_distance
#' @param tolerance The maximum separation of vertices that should
#'   be considered equal.
#'
#' @return A logical vector along the recycled length of `geom1` and `geom2`
#' @export
#'
geos_disjoint <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_disjoint, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_touches <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_touches, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_intersects <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_intersects, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_crosses <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_crosses, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_within <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_within, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_contains <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_contains, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_overlaps <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_overlaps, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_equals <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_equals, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_equals_exact <- function(geom1, geom2, tolerance = .Machine$double.eps ^ 2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2), as.numeric(tolerance)))
  .Call(geos_c_equals_exact, recycled[[1]], recycled[[2]], recycled[[3]])
}

#' @rdname geos_disjoint
#' @export
geos_covers <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_covers, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_covered_by <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_covered_by, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_disjoint <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_disjoint, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_touches <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_touches, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_intersects <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_intersects, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_crosses <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_crosses, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_within <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_within, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_contains <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_contains, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_contains_properly <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_contains_properly, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_overlaps <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_overlaps, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_covers <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_covers, recycled[[1]], recycled[[2]])
}

#' @rdname geos_disjoint
#' @export
geos_prepared_covered_by <- function(geom1, geom2) {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  .Call(geos_c_prepared_covered_by, recycled[[1]], recycled[[2]])
}


#' Dimensionally extended 9 intersection model
#'
#' See the [Wikipedia entry on DE-9IM](https://en.wikipedia.org/wiki/DE-9IM)
#' for how to interpret `pattern`, `match`, and the result of [geos_relate()]
#' and/or [geos_relate_pattern_create()].
#'
#' @inheritParams geos_distance
#' @param boundary_node_rule One of "mod2", "endpoint", "multivalent_endpoint",
#'  or "monovalent_endpoint".
#' @param pattern,match A character vector representing the match
#' @param II,IB,IE,BI,BB,BE,EI,EB,EE One of "0", "1", "2", "T", "F", or "*"
#'   describing the dimension of the intersection between the interior (I),
#'   boundary (B), and exterior (E).
#'
#' @export
#'
geos_relate <- function(geom1, geom2, boundary_node_rule = "mod2") {
  recycled <- recycle_common(list(as_geos_geometry(geom1), as_geos_geometry(geom2)))
  bnr_choices <- c("mod2", "endpoint", "multivalent_endpoint", "monovalent_endpoint")
  boundary_node_rule <- match.arg(boundary_node_rule, bnr_choices)

  .Call(geos_c_relate, recycled[[1]], recycled[[2]], match(boundary_node_rule, bnr_choices))
}

#' @rdname geos_relate
#' @export
geos_relate_pattern <- function(geom1, geom2, pattern, boundary_node_rule = "mod2") {
  geos_relate_pattern_match(
    geos_relate(geom1, geom2, boundary_node_rule = boundary_node_rule),
    pattern
  )
}

#' @rdname geos_relate
#' @export
geos_relate_pattern_match <- function(match, pattern) {
  recycled <- recycle_common(list(as.character(match), as.character(pattern)))
  .Call(geos_c_relate_pattern_match, recycled[[1]], recycled[[2]])
}

#' @rdname geos_relate
#' @export
geos_relate_pattern_create <- function(II = "*", IB = "*", IE = "*",
                                       BI = "*", BB = "*", BE = "*",
                                       EI = "*", EB = "*", EE = "*") {
  args <- list(II, IB, IE, BI, BB, BE, EI, EB, EE)
  args <- lapply(args, geos_relate_pattern_check_item)
  recycled <- recycle_common(args)
  recycled_is_na <- Reduce("|", lapply(recycled, is.na))
  result <- do.call(paste0, recycled)
  result[recycled_is_na] <- NA_character_
  result
}

geos_relate_pattern_check_item <- function(item) {
  item <- as.character(item)
  if (!all(item %in% c("0", "1", "2", "T", "F", "*", NA))) {
    stop("All pattern characters must be one of '0', '1', '2', 'T', 'F', or '*'", call. = FALSE)
  }
  item
}
