
#' Create a GEOS STRTree
#'
#' @inheritParams geos_read_wkt
#' @param tree A [geos_strtree()]
#' @param x An object to convert to a [geos_strtree()]
#' @param ... Unused
#'
#' @return A geos_str_tree object
#' @export
#'
geos_strtree <- function(geom) {
  geom <- as_geos_geometry(geom)
  structure(
    .Call(geos_c_strtree_create, geom),
    class = "geos_strtree",
    crs = attr(geom, "crs", exact = TRUE)
  )
}

#' @rdname geos_strtree
#' @export
geos_strtree_query <- function(tree, geom) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_strtree_query, tree, geom)
}

#' @rdname geos_strtree
#' @export
geos_strtree_data <- function(tree) {
  # doesn't make sense to coerce here because then the output == tree
  stopifnot(inherits(tree, "geos_strtree"))

  .Call(geos_c_strtree_data, as_geos_strtree(tree))
}

#' @rdname geos_strtree
#' @export
as_geos_strtree <- function(x, ...) {
  UseMethod("as_geos_strtree")
}

#' @rdname geos_strtree
#' @export
as_geos_strtree.default <- function(x, ...) {
  as_geos_strtree(as_geos_geometry(x), ...)
}

#' @rdname geos_strtree
#' @export
as_geos_strtree.geos_strtree <- function(x, ...) {
  x
}

#' @rdname geos_strtree
#' @export
as_geos_strtree.geos_geometry <- function(x, ...) {
  geos_strtree(x)
}

#' @export
format.geos_strtree <- function(x, ...) {
  data <- geos_strtree_data(x)
  n_items <- length(data)

  crs <- attr(x, "crs", exact = TRUE)
  if (is.null(crs)) {
    sprintf(
      "<geos_strtree containing %s item%s>",
      n_items, if(n_items == 1) "" else "s"
    )
  } else {
    sprintf(
      "<geos_strtree containing %s item%s with CRS=%s>",
      n_items, if(n_items == 1) "" else "s", format(crs)
    )
  }
}

#' @export
print.geos_strtree <- function(x, ...) {
  cat(paste0(format(x), "\n"))
  invisible(x)
}


#' Matrix predicates
#'
#' @inheritParams geos_strtree
#' @inheritParams geos_disjoint
#' @export
#'
#' @return A `list()` of integer vectors containing the indices of `tree`
#'   for which the predicate would return TRUE.
#'
geos_disjoint_matrix <- function(geom, tree) {
  # disjoint is the odd one out, in that it requires a negation of intersects
  # this is easier to maintain
  # with setdiff() here (unless somebody complains that this is slow)
  tree <- as_geos_strtree(tree)
  tree_data <- geos_strtree_data(tree)
  intersects_matrix <- geos_intersects_matrix(geom, tree)
  Map(setdiff, list(seq_along(tree_data)), intersects_matrix)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_touches_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_touches_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_intersects_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_intersects_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_crosses_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_crosses_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_within_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_within_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_contains_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_contains_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_contains_properly_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_contains_properly_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_overlaps_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_overlaps_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_equals_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_equals_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_equals_exact_matrix <- function(geom, tree, tolerance = .Machine$double.eps ^ 2) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  recycled <- recycle_common(list(geom, as.numeric(tolerance)))
  .Call(geos_c_equals_exact_matrix, recycled[[1]], tree, recycled[[2]])
}

#' @rdname geos_disjoint_matrix
#' @export
geos_covers_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_covers_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_covered_by_matrix <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)
  .Call(geos_c_covered_by_matrix, geom, tree)
}

#' @rdname geos_disjoint_matrix
#' @export
geos_disjoint_any <- function(geom, tree) {
  # disjoint is the odd one out, in that it requires a negation of intersects
  !.Call(geos_c_predicate_any, geos_intersects_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_touches_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_touches_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_intersects_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_intersects_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_crosses_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_crosses_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_within_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_within_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_contains_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_contains_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_contains_properly_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_contains_properly_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_overlaps_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_overlaps_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_equals_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_equals_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_equals_exact_any <- function(geom, tree, tolerance = .Machine$double.eps ^ 2) {
  .Call(geos_c_predicate_any, geos_equals_exact_matrix(geom, tree, tolerance = tolerance))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_covers_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_covers_matrix(geom, tree))
}

#' @rdname geos_disjoint_matrix
#' @export
geos_covered_by_any <- function(geom, tree) {
  .Call(geos_c_predicate_any, geos_covered_by_matrix(geom, tree))
}


#' Find the closest feature
#'
#' Finds the closest item index in `tree` to `geom`, vectorized along `geom`.
#'
#' @inheritParams geos_strtree
#' @inheritParams geos_distance
#'
#' @return An integer vector of length `geom` containing the index
#'   of `tree` that is closest to each feature in `geom`.
#' @export
#'
geos_nearest <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)

  .Call(geos_c_nearest, as_geos_geometry(geom), as_geos_strtree(tree))
}

#' @rdname geos_nearest
#' @export
geos_nearest_indexed <- function(geom, tree) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)

  .Call(geos_c_nearest_indexed, as_geos_geometry(geom), as_geos_strtree(tree))
}

#' @rdname geos_nearest
#' @export
geos_nearest_hausdorff <- function(geom, tree, densify = NULL) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)

  if (is.null(densify)) {
    .Call(geos_c_nearest_hausdorff, as_geos_geometry(geom), as_geos_strtree(tree))
  } else {
    desnify <- as.numeric(densify)
    .Call(geos_c_nearest_hausdorff_densify, as_geos_geometry(geom), as_geos_strtree(tree), densify[1])
  }
}

#' @rdname geos_nearest
#' @export
geos_nearest_frechet <- function(geom, tree, densify = NULL) {
  tree <- as_geos_strtree(tree)
  geom <- as_geos_geometry(geom)
  wk_crs_output(tree, geom)

  if (is.null(densify)) {
    .Call(geos_c_nearest_frechet, as_geos_geometry(geom), as_geos_strtree(tree))
  } else {
    desnify <- as.numeric(densify)
    .Call(geos_c_nearest_frechet_densify, as_geos_geometry(geom), as_geos_strtree(tree), densify[1])
  }
}

# for testing...triggers an error that is hard to otherwise trigger
geos_nearest_error <- function(geom, tree) {
  .Call(geos_c_nearest_error, as_geos_geometry(geom), as_geos_strtree(tree))
}
