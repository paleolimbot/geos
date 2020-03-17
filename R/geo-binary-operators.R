
geo_intersection <- function(x, y, to = geo_ptype(x)) {
  geo_restore(to, geomcpp_intersection(x, y, to))
}
