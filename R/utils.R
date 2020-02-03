
stop_for_non_parseable <- function(is_parseable) {
  if (!all(is_parseable)) {
    bad_geometries <- paste(utils::head(which(!is_parseable), 20), collapse = ", ")
    n_bad_geometries <- sum(!is_parseable)
    if (n_bad_geometries > 20) {
      abort(
        sprintf(
          "%s geometries failed to parse:\n  %s\n  ...and %s more",
          n_bad_geometries, bad_geometries, n_bad_geometries - 20
        ),
        class = "parse_error"
      )
    } else {
      abort(
        sprintf(
          "%s geometry(ies) failed to parse:\n  %s",
          n_bad_geometries, bad_geometries
        ),
        class = "parse_error"
      )
    }
  }
}
