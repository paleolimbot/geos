
#' Create a basic GEOS STRTree
#'
#' An experimental alternative to the [geos_strtree()] that provides a more
#' flexible interface and potentially faster spatial joins. Notably,
#' [geos_basic_strtree_insert()] uses [wk::wk_envelope()] instead of
#' [as_geos_geometry()] and does not keep the underlying geometry in memory.
#' For object types like [wk::xy()] with an optimized [wk::wk_envelope()]
#' method, this is very efficient.
#'
#' @param tree A [geos_basic_strtree()]
#' @param node_capacity The maximum number of child nodes that a node may have.
#'   The minimum recommended capacity value is 4. If unsure, use a
#'   default node capacity of 10.
#' @param items Items to add to the tree index
#' @param query Items with which to query the tree
#' @param limit The maximum number of matches in the tree to return
#' @param fill If `TRUE`, always returns `limit` matches per item in `query`
#'   padded with `NA` if fewer than `limit` matches are found.
#' @param tree_geom A vctr coercible to [geos_geometry()] whose indices
#'   align with `tree`.
#' @param fun A vectorized binary predicate (e.g. [geos_intersects()]) that
#'   will be called with the tree geometry, the query geometry and any `...`
#'   args passed.
#' @param ... Passed to `fun`.
#' @param .chunk_size The approximate number of comparisons to pass to `fun`.
#'
#' @return A geos_basic_strtree object
#' @export
#'
#' @examples
#' tree <- geos_basic_strtree(wk::xy(1:5, 1:5))
#' geos_basic_strtree_size(tree)
#' (geos_basic_strtree_insert(tree, wk::xy(6:10, 6:10)))
#' geos_basic_strtree_query(tree, as_geos_geometry("LINESTRING (3 0, 0 3)"))
#'
geos_basic_strtree <- function(items = NULL, node_capacity = 10L) {
  node_capacity <- sanitize_integer_scalar(node_capacity)
  stopifnot(
    length(node_capacity) == 1,
    node_capacity >= 4, node_capacity < 1e6
  )

  tree <- structure(
    .Call(geos_c_basic_strtree_create, node_capacity),
    class = "geos_basic_strtree"
  )

  if (!is.null(items)) {
    geos_basic_strtree_insert(tree, items)
  }

  tree
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
  items <- unclass(wk::wk_envelope(items))
  result <- .Call(
    geos_c_basic_strtree_insert_rect,
    tree,
    items[[1]], items[[2]], items[[3]], items[[4]]
  )

  if (result[2] == 0) {
    invisible(integer())
  } else {
    invisible(seq(result[1], result[1] + result[2] - 1L))
  }
}

#' @rdname geos_basic_strtree
#' @export
geos_basic_strtree_query <- function(tree, query, limit = NA, fill = FALSE) {
  stopifnot(inherits(tree, "geos_basic_strtree"))
  query <- unclass(wk::wk_envelope(query))

  new_data_frame(
    .Call(
      geos_c_basic_strtree_query_geom,
      tree,
      query[[1]],
      query[[2]],
      query[[3]],
      query[[4]],
      as.integer(limit)[1],
      as.logical(fill)[1]
    )
  )
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
