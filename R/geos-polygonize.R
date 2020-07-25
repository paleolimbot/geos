
#' Create polygons from noded edges
#'
#' @param collection A GEOMETRYCOLLECTION or MULTILINESTRING of
#'   edges that meet at their endpoints.
#'
#' @return A GEOMETRYCOLLECTION of polygons
#' @export
#'
#' @examples
#' geos_polygonize("MULTILINESTRING ((0 0, 0 1), (0 1, 1 0), (1 0, 0 0))")
#'
geos_polygonize <- function(collection) {
  collection <- as_geos_geometry(collection)
  stopifnot(length(collection) == 1)

  new_geos_geometry(list(.Call(geos_c_polygonize, unclass(collection)[[1]])))
}
