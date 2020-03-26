
#' Calculate the intersection of two segments
#'
#' @param x,y Two [geo_segment()]s
#'
#' @return A [geo_xy()]
#' @export
#'
#' @examples
#' geos_segment_intersection(
#'   geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
#'   geo_segment(geo_xy(10, 0), geo_xy(0, 10))
#' )
#'
geos_segment_intersection <- function(x, y) {
  cpp_segment_intersection(
    field(field(x, "start"), "x"), field(field(x, "start"), "y"),
    field(field(x, "end"), "x"), field(field(x, "end"), "y"),
    field(field(y, "start"), "x"), field(field(y, "start"), "y"),
    field(field(y, "end"), "x"), field(field(y, "end"), "y")
  )
}

