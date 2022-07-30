
#' Create a basic GEOS STRTree
#'
#' @param tree A [geos_basic_strtree()]
#' @param node_capacity The maximum number of child nodes that a node may have.
#'   The minimum recommended capacity value is 4. If unsure, use a
#'   default node capacity of 10.
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
