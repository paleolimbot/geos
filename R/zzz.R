
# nocov start
.onLoad <- function(...) {
  # Load libgeos namespace for access to C callables
  requireNamespace("libgeos", quietly = TRUE)

  # Initialize geos C globals
  .Call(c_geos_init)
}
# nocov end
