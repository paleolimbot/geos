
assert_geos_version <- function(version) {
  if (geos_version() < version) {
    abort(sprintf("GEOS >= %s is required", version))
  }
}

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

n_distinct <- function(x) {
  length(unique(x))
}

rep_along_or_fail <- function(x, template) {
  x_quo <- rlang::enquo(x)
  template_quo <- rlang::enquo(template)

  if (vec_size(x) == 1) {
    vec_repeat(x, times = vec_size(template))
  } else if (vec_size(x) != vec_size(template)) {
    x_label <- rlang::as_label(x_quo)
    abort(
      sprintf(
        "`%s` must be length 1 or the same length as `%s` (%s)",
        rlang::as_label(x_quo), rlang::as_label(template_quo), vec_size(template)
      ),
      class = "rep_len_error"
    )
  } else {
    x
  }
}

rep_len_or_fail <- function(x, size) {
  x_quo <- rlang::enquo(x)

  if (size == 1 || vec_size(x) == size) {
    x
  } else if (vec_size(x) == 1) {
    vec_repeat(x, times = size)
  } else {
    x_label <- rlang::as_label(x_quo)
    abort(
      sprintf(
        "`%s` must be length 1 or %s",
        rlang::as_label(x_quo), size
      ),
      class = "rep_len_error"
    )
  }
}
