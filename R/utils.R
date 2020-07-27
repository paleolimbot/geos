
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

new_data_frame <- function(x, nrow = length(x[[1]])) {
  structure(x, class = "data.frame", row.names = seq_len(nrow))
}
