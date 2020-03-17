
#' Intersect two geometry vectors
#'
#' @inheritParams geo_ptype
#' @param y A geometry-like object
#'
#' @return A geometry-like object, in the format defined by `to`.
#' @export
#'
#' @examples
#' poly1 <- geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
#' poly2 <- geo_wkt("POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))")
#'
#' geo_plot(c(poly1, poly2))
#' geo_plot_add(geo_intersection(poly1, poly2), col = "grey90")
#'
geo_intersection <- function(x, y, to = geo_ptype(x)) {
  geo_restore(to, geomcpp_intersection(x, y, to))
}
