
#' Convert a geometry to a different data structure
#'
#' @inheritParams geo_ptype
#'
#' @return A geometry vector, in the format defined by `to`.
#' @export
#'
#' @examples
#' geo_convert(geo_wkt("POINT (20 10)"), geo_wkb())
#' geo_convert(geo_wkt("POINT (20 10)"), geo_coord())
#'
geo_convert <- function(x, to) {
  geo_restore(to, geomcpp_convert(x, to))
}

#' Buffer a geometry
#'
#' Vectorized along `x` and `width`.
#'
#' @inheritParams geo_ptype
#' @param width The buffer distance. "Width" is the GEOS term - it is more
#'   accurately thought of as "radius" or "half-width".
#' @param quad_segs The number of segments per quadrant. A higher number
#'   here will increase the apparent resolution of the resulting polygon.
#' @param end_cap_style One of "round", "flat", or "square".
#' @param join_style One of "round", "mitre", or "bevel".
#' @param mitre_limit If `join_style` is "mitre", the relative extent (from zero to one)
#'   of the join.
#' @param single_sided Use `TRUE` to buffer on only the right side
#'   of the geometry.
#'
#' @return A geometry-like polygon or multipolygon, in the format defined by `to`.
#' @export
#'
#' @examples
#' point <- geo_wkt("POINT (0 0)")
#' geo_plot(geos_buffer(point, width = 0.5))
#' geo_plot_add(point)
#'
geos_buffer <- function(x, width, quad_segs = 30,
                       end_cap_style = c("round", "flat", "square"),
                       join_style = c("round", "mitre", "bevel"),
                       mitre_limit = 1,
                       single_sided = FALSE,
                       to = geo_ptype(x)) {
  end_cap_style <- match.arg(end_cap_style)
  end_cap_style_int <- match(end_cap_style, c("round", "flat", "square"))

  join_style <- match.arg(join_style)
  join_style_int <- match(join_style, c("round", "mitre", "bevel"))

  width <- rep_len_or_fail(width, geo_size(x))

  result <- geomcpp_buffer(
    x, to,
    width = width,
    quadSegs = quad_segs,
    endCapStyle = end_cap_style_int,
    joinStyle = join_style_int,
    mitreLimit = mitre_limit,
    singleSided = single_sided
  )

  geo_restore(to, result)
}
