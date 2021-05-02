
# slightly faster than as*() and better communicates intention
# in the future these could error for spurious inputs / be implemented in C
sanitize_geos_geometry <- function(x) {
  if (inherits(x, "geos_geometry")) x else as_geos_geometry(x)
}

sanitize_geos_strtree <- function(x) {
  if (inherits(x, "geos_strtree")) x else as_geos_strtree(x)
}

sanitize_double <- function(x) {
  as.numeric(x)
}

sanitize_integer <- function(x) {
  as.integer(x)
}

sanitize_double_scalar <- function(x) {
  as.numeric(x)[1]
}

sanitize_integer_scalar <- function(x) {
  as.integer(x)[1]
}

sanitize_logical_scalar <- function(x) {
  as.logical(x)[1]
}

recycle_common <- function(dots) {
  final_length <- check_lengths(dots)
  lapply(dots, rep_len, final_length)
}

check_lengths <- function(dots) {
  lengths <- vapply(dots, length, integer(1))
  non_constant_lengths <- unique(lengths[lengths != 1])
  if (length(non_constant_lengths) == 0) {
    1
  } else if(length(non_constant_lengths) == 1) {
    non_constant_lengths
  } else {
    lengths_label <- paste0(non_constant_lengths, collapse = ", ")
    stop(sprintf("Incompatible lengths: %s", lengths_label), call. = FALSE)
  }
}

new_data_frame <- function(x) {
  structure(x, row.names = c(NA, length(x[[1]])), class = "data.frame")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
