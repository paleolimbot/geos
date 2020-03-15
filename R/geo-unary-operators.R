
#' Buffer a geometry
#'
#' @inheritParams geo_ptype
#' @param width The buffer distance
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
geo_buffer <- function(x, width, quad_segs = 30,
                       end_cap_style = c("round", "flat", "square"),
                       join_style = c("round", "mitre", "bevel"),
                       mitre_limit = 1,
                       single_sided = FALSE,
                       to = geo_ptype(x)) {
  end_cap_style <- match.arg(end_cap_style)
  end_cap_style_int <- match(end_cap_style, c("round", "flat", "square"))

  join_style <- match.arg(join_style)
  join_style_int <- match(join_style, c("round", "mitre", "bevel"))

  result <- geomcpp_buffer(
    x, to,
    width = width,
    quadSegs = quad_segs,
    endCapStyle = end_cap_style_int,
    joinStyle = join_style_int,
    mitreLimit = mitre_limit,
    singleSided = single_sided
  )

  geo_restore(result, to)
}
