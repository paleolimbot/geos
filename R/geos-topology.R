
#' Geometry topology operators
#'
#' - [geos_centroid()] returns the middle of the bounding box, regardless
#'   of whether or not the point lies within the feature.
#' - [geos_point_on_surface()] returns a point on the feature that is
#'   guaranteed to be within the feature.
#' - [geos_node()] "nodes" the input (linestring or polygon), removing duplicate
#'   nodes that may exist in multiple features
#' - [geos_boundary()] returns the boudnary of the feature. For a polygon,
#'   this will be a linestring; for a linestring, this will be a multipoint.
#' - [geos_envelope()] returns the bounding box of the feature as a
#'   non-rotated rectangle as a polygon.
#' - [geos_convex_hull()] returns the convex hull in the simplest form
#'   possible, which may be a point or a linestring.
#' - [geos_minimum_rotated_rectangle()] returns the smallest possible rectangle
#'   that completely contains the `x`.
#' - [geos_minimum_bounding_circle()] returns the smallest possible rectangle
#'   that completely contains the `x`. The circle isn't particularly high
#'   resolution, but you can construct it yourself using center and radius
#'   provided by [geos_minimum_bounding_circle_center()] and
#'   [geos_minimum_bounding_circle_radius()], respectively.
#' - [geos_minimum_width()] returns the smallest possible "hole" (as a linestring)
#'   that the geometry can fit through with a single rotation. If the geometry is
#'   the couch, the length of this line is that you would measure before trying
#'   to squeeze it through a narrow hallway.
#' - [geos_minimum_clearance_line()] the smallest internal "hole". Here the
#'   geometry is the hallway, and this is the place you have to worry about
#'   when moving your couch through it.
#' - [geos_minimum_clearance()] the length of [geos_minimum_clearance_line()].
#'   If the minimum width of your couch is larger than this value, do not try to
#'   move your couch through this geometry.
#'
#' @inheritParams geo_ptype
#' @return A geometry vector, except [geos_minimum_clearance()], which
#'   returns a numeric vector.
#'
#' @export
#'
#' @examples
#' line <- geo_wkt("LINESTRING (30 10, 10 30, 40 40)")
#' poly <- geo_wkt("
#' POLYGON (
#'   (35 10, 45 45, 15 40, 10 20, 35 10),
#'   (20 30, 35 35, 30 20, 20 30)
#' )
#' ")
#'
#' # centroid, point on surface for polygon
#' geo_plot(poly)
#' geo_plot_add(geos_centroid(poly), col = "red")
#' geo_plot_add(geos_point_on_surface(poly), col = "blue")
#'
#' # centroid, point on surface for line
#' geo_plot(line)
#' geo_plot_add(geos_centroid(line), col = "red")
#' geo_plot_add(geos_point_on_surface(line), col = "blue")
#'
#' # "noding"
#' geos_node(geo_wkt("MULTILINESTRING ((0 0, 10 10), (0 0, 10 10))"))
#'
#' # boundary
#' geo_plot(poly)
#' geo_plot(geos_boundary(poly), col = "red")
#'
#' # envelope
#' geo_plot(geos_envelope(poly), col = "grey90")
#' geo_plot_add(poly)
#'
#' # convex hull
#' geo_plot(geos_convex_hull(line), col = "grey90", border = NA)
#' geo_plot_add(line)
#'
#' # minimum rotated rectangle
#' geo_plot(
#'   geos_minimum_rotated_rectangle(
#'     poly
#'   ),
#'   col = "grey90",
#'   border = NA
#' )
#' geo_plot_add(poly)
#'
#' # minimum bounding circle (only with GEOS >= 3.8.0)
#' if (geos_version() >= "3.8.0") {
#'   geo_plot(
#'     geos_minimum_bounding_circle(
#'       poly
#'     ),
#'     col = "grey90",
#'     border = NA
#'   )
#'   geo_plot_add(poly)
#'   geo_plot_add(
#'     geos_minimum_bounding_circle_center(
#'       poly
#'     ),
#'     col = "red"
#'   )
#'
#'   geos_minimum_bounding_circle_radius(poly)
#' }
#'
#' # minimum width
#' geo_plot(poly)
#' geo_plot_add(geos_minimum_width(poly), col = "red")
#'
#' # minimum clearance line
#' geo_plot(poly)
#' geo_plot_add(geos_minimum_clearance_line(poly), col = "red")
#' geos_minimum_clearance(poly)
#'
geos_centroid <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_centroid(x, to))
}

#' @rdname geos_centroid
#' @export
geos_point_on_surface <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_point_on_surface(x, to))
}

#' @rdname geos_centroid
#' @export
geos_node <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_node(x, to))
}

#' @rdname geos_centroid
#' @export
geos_boundary <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_boundary(x, to))
}

#' @rdname geos_centroid
#' @export
geos_envelope <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_envelope(x, to))
}

#' @rdname geos_centroid
#' @export
geos_convex_hull <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_convex_hull(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_rotated_rectangle <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_minimum_rotated_rectangle(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_bounding_circle <- function(x, to = geo_ptype(x)) {
  assert_geos_version("3.8.0")
  geo_restore(to, cpp_minimum_bounding_circle(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_bounding_circle_radius <- function(x) {
  assert_geos_version("3.8.0")
  cpp_minimum_bounding_circle_radius(x)
}

#' @rdname geos_centroid
#' @export
geos_minimum_bounding_circle_center <- function(x, to = geo_ptype(x)) {
  assert_geos_version("3.8.0")
  geo_restore(to, cpp_minimum_bounding_circle_center(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_width <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_minimum_width(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_clearance_line <- function(x, to = geo_ptype(x)) {
  geo_restore(to, cpp_minimum_clearance_line(x, to))
}

#' @rdname geos_centroid
#' @export
geos_minimum_clearance <- function(x) {
  cpp_minimum_clearance(x)
}
