
#' Plot GEOS geometries
#'
#' @param x A [GEOS geometry vector][as_geos_geometry]
#' @inheritParams wkutils::wkb_plot
#'
#' @return The input, invisibly
#' @export
#'
#' @examples
#' plot(as_geos_geometry("LINESTRING (0 0, 1 1)"))
#' plot(as_geos_geometry("POINT (0.5 0.4)"), add = TRUE)
#'
plot.geos_geometry <- function(x, ..., asp = 1, bbox = NULL, xlab = "", ylab = "",
                               rule = "evenodd", add = FALSE) {
  wkutils::wkb_plot(
    wk::as_wkb(x, include_srid = FALSE, include_z = FALSE),
    ...,
    asp = asp,
    bbox = bbox,
    xlab = xlab,
    ylab = ylab,
    rule = rule,
    add = add
  )

  invisible(x)
}
