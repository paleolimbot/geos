
#' GEOS version information
#'
#' @param runtime Use FALSE to return the build-time
#'   GEOS version, which may be different than the runtime
#'   version if a different version of the
#'   [libgeos package][libgeos::libgeos_version] was used to build
#'   this package.
#' @export
#'
#' @examples
#' geos_version()
#' geos_version(runtime = FALSE)
#'
#' # check for a minimum version of GEOS
#' geos_version() >= "3.8.1"
#'
geos_version <- function(runtime = TRUE) {
  version <- if (runtime) geos_version_base_runtime() else geos_version_base_build()
  package_version(strsplit(version, "[- ]")[[1]][1])
}

geos_version_base_runtime <- function() {
  .Call(geos_c_version_runtime)
}

geos_version_base_build <- function() {
  .Call(geos_c_version_build)
}
