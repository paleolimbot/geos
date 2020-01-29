
#' GEOS information
#'
#' @export
#'
#' @examples
#' geos_version()
#' geos_capi_version()
#'
geos_version <- function() {
  strsplit(geos_version_impl(), "[- ]")[[1]][1]
}

#' @export
#' @rdname geos_version
geos_capi_version <- function() {
  strsplit(geos_version_impl(), "[- ]")[[1]][3]
}
