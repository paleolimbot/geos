
#' GEOS information
#'
#' @export
#'
#' @examples
#' geos_version()
#' geos_capi_version()
#'
geos_version <- function() {
  package_version(strsplit(cpp_version_impl(), "[- ]")[[1]][1])
}

#' @export
#' @rdname geos_version
geos_capi_version <- function() {
  package_version(strsplit(cpp_version_impl(), "[- ]")[[1]][3])
}

.stop_geos <- function(msg) {
  on.exit(abort(msg, class = "geos_error"))
  lst <- strsplit(msg, " at ")[[1]]
  pts <- scan(text = lst[[length(lst)]], quiet = TRUE)
  if (length(pts) == 2 && is.numeric(pts)) {
    assign(".geos_error", pts, envir = .geom_cache)
  }
}

.geom_cache <- new.env(parent = emptyenv())
