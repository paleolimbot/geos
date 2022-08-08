
#' Create a basic GEOS STRTree
#'
#' @param tree A [geos_basic_strtree()]
#' @param node_capacity The maximum number of child nodes that a node may have.
#'   The minimum recommended capacity value is 4. If unsure, use a
#'   default node capacity of 10.
#' @param items Items to add to the tree index
#' @param query Items with which to query the tree
#' @param tree_geom A vctr coercible to [geos_geometry()] whose indices
#'   align with `tree`.
#' @param fun A vectorized binary predicate (e.g. [geos_intersects()]) that
#'   will be called with the tree geometry, the query geometry and any `...`
#'   args passed.
#' @param ... Passed to `fun`.
#'
#' @return A geos_basic_strtree object
#' @export
#'
geos_basic_strtree <- function(node_capacity = 10L) {
  node_capacity <- sanitize_integer_scalar(node_capacity)
  stopifnot(
    length(node_capacity) == 1,
    node_capacity >= 4, node_capacity < 1e6
  )

  structure(
    .Call(geos_c_basic_strtree_create, node_capacity),
    class = "geos_basic_strtree"
  )
}

#' @rdname geos_basic_strtree
#' @export
geos_basic_strtree_size <- function(tree) {
  stopifnot(inherits(tree, "geos_basic_strtree"))
  .Call(geos_c_basic_strtree_size, tree)
}

#' @rdname geos_basic_strtree
#' @export
geos_basic_strtree_finalized <- function(tree) {
  stopifnot(inherits(tree, "geos_basic_strtree"))
  .Call(geos_c_basic_strtree_finalized, tree)
}

#' @rdname geos_basic_strtree
#' @export
geos_basic_strtree_insert <- function(tree, items) {
  stopifnot(inherits(tree, "geos_basic_strtree"))
  if (inherits(items, "geos_geometry")) {
    result <- .Call(geos_c_basic_strtree_insert_geom, tree, items)
  } else {
    items <- unclass(wk::wk_envelope(items))
    result <- .Call(
      geos_c_basic_strtree_insert_rect,
      tree,
      items[[1]], items[[2]], items[[3]], items[[4]]
    )
  }

  if (result[2] == 0) {
    invisible(integer())
  } else {
    invisible(seq(result[1], result[1] + result[2] - 1L))
  }
}

#' @rdname geos_basic_strtree
#' @export
geos_basic_strtree_query <- function(tree, query) {
  stopifnot(inherits(tree, "geos_basic_strtree"))
  if (!inherits(query, "geos_geometry")) {
    query <- as_geos_geometry(wk::wk_envelope(query))
  }

  new_data_frame(.Call(geos_c_basic_strtree_query_geom, tree, query))
}

#' @rdname geos_basic_strtree
#' @export
geos_basic_strtree_query_filtered <- function(tree, query, tree_geom, fun, ...,
                                              .chunk_size = 65536) {
  keys <- geos_basic_strtree_query(tree, query)
  if (nrow(keys) == 0) {
    return(keys)
  }

  keys_filter <- logical(nrow(keys))
  chunk_strategy <- wk::wk_chunk_strategy_feature(chunk_size = .chunk_size)
  chunks <- chunk_strategy(keys$tree, nrow(keys))
  for (i in seq_len(nrow(chunks))) {
    range <- (chunks$from[i]):(chunks$to[i])
    keys_filter[range] <- fun(
      tree_geom[keys$tree[range]],
      query[keys$x[range]],
      ...
    )
  }

  keys[keys_filter, , drop = FALSE]
}
