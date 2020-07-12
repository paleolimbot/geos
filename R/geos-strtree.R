
#' Create a GEOS STRTree
#'
#' @inheritParams geos_read_wkt
#' @param tree A [geos_str_tree()]
#' @param x An object to convert to a [geos_strtree()]
#'
#' @return A geos_str_tree object
#' @export
#'
geos_strtree <- function(geom) {
  structure(.Call(geos_c_strtree_create, as_geos_geometry(geom)), class = "geos_strtree")
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

#' @rdname geos_strtree
#' @export
geos_strtree_data <- function(tree) {
  # doesn't make sense to coerce here because then the output == tree
  stopifnot(inherits(tree, "geos_strtree"))

  .Call(geos_c_strtree_data, as_geos_strtree(tree))
}
