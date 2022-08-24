
#' Generate inner join keys based on a GEOS predicate
#'
#' Experimental low-level spatial join infrastructure based on the
#' [geos_basic_strtree()].
#'
#' @param x,y Geometry vectors with a [wk::wk_handle()] method.
#' @param predicate One of:
#'   - intersects
#'   - contains
#'   - contains_properly
#'   - covered_by
#'   - covers
#'   - crosses
#'   - equals
#'   - equals_exact
#'   - intersects
#'   - within_distance
#'   - overlaps
#'   - touches
#' @param distance Passed to [geos_is_within_distance()] when `predicate`
#'   is "within_distance"; passed to [geos_equals_exact()] when `predicate`
#'   is "equals_exact.
#' @param suffix A character vector of length 2 with suffixes for the left
#'   and right sides for output columns with duplicated names.
#'
#' @return A data.frame with columns x and y corresponding to the 1-based
#'   indices on pairs of `x` and `y` for which `predicate` is TRUE.
#' @export
#'
#' @examples
#' x <- data.frame(
#'   col_x = "a",
#'   geometry = as_geos_geometry("POINT (10 10)")
#' )
#'
#' y <- data.frame(
#'   col_y = "a",
#'   geometry = as_geos_geometry("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
#' )
#'
#' geos_inner_join(x, y, "intersects")
#'
#' geos_inner_join_keys(
#'   "POINT (5 5)",
#'   "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
#'   "intersects"
#' )
#'
geos_inner_join <- function(x, y, predicate = "intersects", distance = NA,
                            suffix = c(".x", ".y")) {
  stopifnot(is.data.frame(x), is.data.frame(y))
  x_geom <- handleable_vector(x, "x")
  y_geom <- handleable_vector(y, "y")
  keys <- geos_inner_join_keys(x_geom, y_geom, predicate, distance)

  left <- x[keys$x, , drop = FALSE]
  right <- y[keys$y, , drop = FALSE]
  left_name_needs_suffix <- names(left) %in% names(right)
  right_name_needs_suffix <- names(right) %in% names(left)

  names(left)[left_name_needs_suffix] <-
    paste0(names(left)[left_name_needs_suffix], suffix[1])
  names(right)[right_name_needs_suffix] <-
    paste0(names(right)[right_name_needs_suffix], suffix[2])

  new_data_frame(c(unclass(left), unclass(right)))
}

#' @rdname geos_inner_join
#' @export
geos_inner_join_keys <- function(x, y, predicate = "intersects", distance = NA) {
  stopifnot(is.character(predicate), length(predicate) == 1)

  # within is usually faster building the index x
  if (identical(predicate, "within")) {
    keys <- geos_inner_join_keys(y, x, "contains")
    return(new_data_frame(list(x = keys$y, y = keys$x)))
  }

  # hedge a bet that the non-indexed geometry will be faster to convert
  # to geos_geometry up front rather than chunk-wise later on
  x <- sanitize_geos_geometry(x)

  # the y geometry is converted lazily but a straight character isn't supported
  # by wk_handle()
  if (is.character(y)) {
    y <- as_geos_geometry(y)
  }

  wk::wk_crs_output(x, y)

  # build the index on y (which requires a slight modification for some
  # predicates)
  if (predicate %in% c("within_distance", "equals_exact")) {
    stopifnot(!identical(distance, NA))
    envelope <- unclass(wk::wk_envelope(y))
    envelope$xmin <- envelope$xmin - distance
    envelope$ymin <- envelope$ymin - distance
    envelope$xmax <- envelope$xmax + distance
    envelope$ymax <- envelope$ymax + distance
    tree <- geos_basic_strtree(wk::new_wk_rct(envelope))
  } else {
    tree <- geos_basic_strtree(y)
  }

  fun <- switch(
    predicate,
    contains = geos_prepared_contains,
    contains_properly = geos_prepared_contains_properly,
    covered_by = geos_prepared_covered_by,
    covers = geos_prepared_covers,
    crosses = geos_prepared_crosses,
    equals = geos_equals,
    equals_exact = function(x, y) geos_equals_exact(x, y, distance),
    intersects = geos_prepared_intersects,
    within_distance = function(x, y) geos_prepared_is_within_distance(x, y, distance),
    overlaps = geos_prepared_overlaps,
    touches = geos_prepared_touches,
    stop(sprintf("Unknown predicate '%s'", predicate))
  )

  result <- geos_basic_strtree_query_filtered(tree, x, y, fun)
  names(result) <- c("x", "y")
  result
}

handleable_vector <- function(x, arg = "x") {
  if (is.data.frame(x)) {
    is_handleable <- vapply(x, is_handleable_column, logical(1))
    if (!any(is_handleable)) {
      stop(sprintf("No geometry column was found in `%s`", arg))
    }

    handleable_vector(x[[which(is_handleable)[1]]])
  } else {
    x
  }
}

is_handleable_column <- function(x) {
  tryCatch({
    wk::wk_vector_meta(x)
    TRUE
  }, error = function(e) FALSE)
}
