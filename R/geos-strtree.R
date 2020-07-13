
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
  structure(.Call(geos_c_strtree_create, as_geos_geometry(geom)), class = "geos_strtree")
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
geos_strtree_query <- function(tree, geom) {
  .Call(geos_c_strtree_query, as_geos_strtree(tree), as_geos_geometry(geom))
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
  sprintf("<geos_strtree containing %s item%s>", n_items, if(n_items == 1) "" else "s")
}

#' @export
print.geos_strtree <- function(x, ...) {
  cat(paste0(format(x), "\n"))
  invisible(x)
}

