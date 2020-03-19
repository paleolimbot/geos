
#' Plot geometry vectors
#'
#' Currently, only geometries of the same type can be plotted in the same
#' plot call (geometries are promoted to mutli-geometries if needed).
#'
#' @inheritParams geo_ptype
#' @param ... Passed to plotting functions for features: [graphics::points()]
#'   for point and multipoint geometries, [graphics::lines()] for linestring
#'   and multilinestring geometries, and [graphics::polypath()] for polygon
#'   and multipolygon geometries.
#' @param asp,xlim,ylim,xlab,ylab Passed to [graphics::plot()]
#' @param rule One of "evenodd" or "winding": controls the appearance of
#'   holes in polygons. See [graphics::polypath()].
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' geo_plot(geo_wkt("POINT (10 40)"))
#' geo_plot(geo_wkt("LINESTRING (30 10, 10 30, 40 40)"))
#' geo_plot(geo_wkt("MULTIPOINT ((10 40), (40 30))"))
#' geo_plot(geo_wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))"))
#' geo_plot(geo_wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))"))
#' geo_plot(
#'   geo_wkt(
#'     "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),
#'       ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))"
#'   ),
#'   col = "grey10"
#' )
#'
geo_plot <- function(x, ..., asp = 1, xlim = NULL, ylim = NULL, xlab = "", ylab = "") {
  # until there is a geo_bbox() function
  if (is.null(xlim) || is.null(ylim)) {
    tbl <- geo_convert_geo_coord_lazy(x)
    xy <- field(tbl, "xy")
    obj <- tbl
  } else {
    obj <- x
  }

  graphics::plot(
    numeric(0),
    numeric(0),
    xlim = xlim %||% range(field(xy, "x")),
    ylim = ylim %||% range(field(xy, "y")),
    xlab = xlab,
    ylab = ylab,
    asp = asp
  )

  geo_plot_add(obj, ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add <- function(x, ...) {
  UseMethod("geo_plot_add")
}

#' @importFrom graphics plot
#' @rdname geo_plot
#' @export
plot.geo_coord <- function(x, ...) {
  geo_plot(x, ...)
}

#' @importFrom graphics plot
#' @rdname geo_plot
#' @export
plot.geo_wkt <- function(x, ...) {
  geo_plot(x, ...)
}

#' @importFrom graphics plot
#' @rdname geo_plot
#' @export
plot.geo_wkb <- function(x, ...) {
  geo_plot(x, ...)
}

#' @rdname geo_plot
#' @export
geo_plot_add.default <- function(x, ...) {
  tbl <- geo_convert_geo_coord_lazy(x)
  geo_plot_add(tbl, ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geo_coord_point <- function(x, ...) {
  xy <- field(x, "xy")
  graphics::points(field(xy, "x"), field(xy, "y"), ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geo_coord_linestring <- function(x, ...) {
  xy <- separate_groups_with_na(field(x, "xy"), field(x, "feature"))
  graphics::lines(field(xy, "x"), field(xy, "y"), ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geo_coord_polygon <- function(x, ..., rule = "evenodd") {
  # have to do one feature at a time because the "holes in polygons" problem
  for (feature in split(x, field(x, "feature"))) {
    xy <- separate_groups_with_na(field(feature, "xy"), field(feature, "piece"))
    graphics::polypath(field(xy, "x"), field(xy, "y"), ..., rule = rule)
  }
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geo_coord_multipoint <- function(x, ...) {
  xy <- field(x, "xy")
  graphics::points(field(xy, "x"), field(xy, "y"), ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geo_coord_multilinestring <- function(x, ...) {
  xy <- separate_groups_with_na(
    field(x, "xy"),
    interaction(field(x, "feature"), field(x, "part"))
  )
  graphics::lines(field(xy, "x"), field(xy, "y"), ...)
  invisible(x)
}

#' @rdname geo_plot
#' @export
geo_plot_add.geo_coord_multipolygon <- function(x, ..., rule = "evenodd") {
  # have to do one part at a time because the "holes in polygons" problem
  for (feature in split(x, interaction(field(x, "feature"),  field(x, "part")))) {
    xy <- separate_groups_with_na(field(feature, "xy"), field(feature, "piece"))
    graphics::polypath(field(xy, "x"), field(xy, "y"), ..., rule = rule)
  }
  invisible(x)
}

# until the laziness of geo_convert is sorted
geo_convert_geo_coord_lazy <- function(x) {
  if (inherits(x, "geo_coord")) {
    x
  } else {
    geo_convert(x, geo_coord())
  }
}

separate_groups_with_na <- function(x, groups) {
  if (length(x) == 0) {
    return(x)
  }

  if (is.factor(groups)) {
    groups <- as.integer(unclass(groups))
  }

  n_groups <- n_distinct(groups)
  lengths <- rle(groups)$lengths

  start_i <- c(0, cumsum(lengths[-length(lengths)]))
  new_start_i <- start_i + seq_along(lengths) - 1
  indices <- Map(function(x, y) c(x + seq_len(y), NA), start_i, lengths)
  x[vec_c(!!!indices)][-(length(x) + n_groups)]
}
