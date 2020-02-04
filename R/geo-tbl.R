
#' Generics common to geo_tbl_*() classes
#'
#' @param x A (possibly) [geo_tbl_point()], [geo_tbl_multipoint()],
#'   [geo_tbl_linestring()], [geo_tbl_multilinestring()],
#'   [geo_tbl_polygon()], or [geo_tbl_multipolygon()], or [geo_xy()]
#' @param y,to See [vctrs::vec_cast()] and [vctrs::vec_ptype2()]
#' @param ... Unused
#'
#' @export
#'
is_geo_tbl <- function(x) {
  inherits(x, "geo_tbl")
}

#' @method vec_ptype2 geo_tbl
#' @export
#' @export vec_ptype2.geo_tbl
#' @rdname is_geo_tbl
vec_ptype2.geo_tbl <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_tbl", y)
}

#' @method vec_ptype2.geo_tbl default
#' @export
vec_ptype2.geo_tbl.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_tbl geo_tbl
#' @export
vec_ptype2.geo_tbl.geo_tbl <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  # always pick 'multi' for common types
  # slightly easier to do it here rather than using a million double dispatch
  # methods
  if (inherits(x, "geo_tbl_point") && inherits(y, "geo_tbl_multipoint")) {
    new_geo_tbl_multipoint()
  } else if(inherits(x, "geo_tbl_multipoint") && inherits(y, "geo_tbl_point")) {
    new_geo_tbl_multipoint()
  } else if (inherits(x, "geo_tbl_linestring") && inherits(y, "geo_tbl_multilinestring")) {
    new_geo_tbl_multilinestring()
  } else if(inherits(x, "geo_tbl_multilinestring") && inherits(y, "geo_tbl_linestring")) {
    new_geo_tbl_multilinestring()
  } else if (inherits(x, "geo_tbl_polygon") && inherits(y, "geo_tbl_multipolygon")) {
    new_geo_tbl_multipolygon()
  } else if(inherits(x, "geo_tbl_multipolygon") && inherits(y, "geo_tbl_polygon")) {
    new_geo_tbl_multipolygon()
  } else {
    NextMethod()
  }
}

#' @method vec_ptype2.geo_tbl data.frame
#' @export
vec_ptype2.geo_tbl.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  as.data.frame(geo_get_ptype_df(x))
}

#' @method vec_ptype2.geo_tbl tbl_df
#' @export
vec_ptype2.geo_tbl.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_get_ptype_df(x)
}

#' @method vec_ptype2.data.frame geo_tbl
#' @export
vec_ptype2.data.frame.geo_tbl <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  as.data.frame(geo_get_ptype_df(y))
}

geo_get_ptype_df <- function(geo_tbl) {
  ptype_default <- tibble(
    x = double(),
    y = double(),
    xy = geo_xy(),
    feature = integer(),
    part = integer(),
    piece = integer()
  )
  ptype_default[names(vec_data(geo_tbl))]
}

#' @export
as_tibble.geo_tbl <- function(x, ...) {
  as_tibble(vec_data(x))
}

#' @export
as.data.frame.geo_tbl <- function(x, ...) {
  as.data.frame(as_tibble(x))
}

#' @method vec_cast geo_tbl
#' @export
#' @export vec_cast.geo_tbl
#' @rdname is_geo_tbl
vec_cast.geo_tbl <- function(x, to, ...) {
  UseMethod("vec_cast.geo_tbl")
}

#' @method vec_cast.geo_tbl default
#' @export
vec_cast.geo_tbl.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_tbl geo_tbl
#' @export
vec_cast.geo_tbl.geo_tbl <- function(x, to, ...) {
  # always pick 'multi' for common types
  # slightly easier to do it here rather than using a million double dispatch
  # methods
  if (inherits(x, "geo_tbl_point") && inherits(to, "geo_tbl_multipoint")) {
    new_geo_tbl_multipoint(vec_data(x))
  } else if (inherits(x, "geo_tbl_linestring") && inherits(to, "geo_tbl_multilinestring")) {
    data <- vec_data(x)
    data$part <- rep_len(1L, vec_size(x))
    new_geo_tbl_multilinestring(data)
  } else if (inherits(x, "geo_tbl_polygon") && inherits(to, "geo_tbl_multipolygon")) {
    data <- vec_data(x)
    data$part <- rep_len(1L, vec_size(x))
    new_geo_tbl_multipolygon(data)
  } else if(identical(class(x), class(to))) {
    x
  } else {
    NextMethod()
  }
}

#' @method vec_cast.geo_tbl data.frame
#' @export
vec_cast.geo_tbl.data.frame <- function(x, to, ...) {
  vec_cast.geo_tbl.list(x, to, ...)
}

#' @method vec_cast.data.frame geo_tbl
#' @export
vec_cast.data.frame.geo_tbl <- function(x, to, ...) {
  as.data.frame(as_tibble(vec_data(x)))
}

#' @method vec_cast.geo_tbl list
#' @export
vec_cast.geo_tbl.list <- function(x, to, ...) {
  to_class <- class(to)[1]
  if (!rlang::is_dictionaryish(x)) {
    abort(sprintf("Can't convert an unnamed list to <%s>", to_class))
  }
  constructor <- rlang::as_function(to_class, env = getNamespace("geom"))
  arg_names <- intersect(names(x), names(formals(constructor)))
  rlang::exec(constructor, !!!x[arg_names])
}

#' @method vec_cast.list geo_tbl
#' @export
vec_cast.list.geo_tbl <- function(x, to, ...) {
  vec_data(x)
}
