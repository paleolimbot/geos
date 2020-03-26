
#' Generics common to geo_coord_*() classes
#'
#' @param x A (possibly) [geo_coord_point()], [geo_coord_multipoint()],
#'   [geo_coord_linestring()], [geo_coord_multilinestring()],
#'   [geo_coord_polygon()], or [geo_coord_multipolygon()], or [geo_xy()]
#' @param y,to See [vctrs::vec_cast()] and [vctrs::vec_ptype2()]
#' @param ... Unused
#'
#' @export
#'
is_geo_coord <- function(x) {
  inherits(x, "geo_coord")
}

#' @rdname is_geo_coord
#' @export
geo_coord <- function() {
  structure(list(), class = "geo_coord")
}

#' @method vec_ptype2 geo_coord
#' @export
#' @export vec_ptype2.geo_coord
#' @rdname is_geo_coord
vec_ptype2.geo_coord <- function(x, y, ...) {
  UseMethod("vec_ptype2.geo_coord", y)
}

#' @method vec_ptype2.geo_coord default
#' @export
vec_ptype2.geo_coord.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.geo_coord geo_coord
#' @export
vec_ptype2.geo_coord.geo_coord <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  # always pick 'multi' for common types
  # slightly easier to do it here rather than using a million double dispatch
  # methods
  if (inherits(x, "geo_coord_point") && inherits(y, "geo_coord_multipoint")) {
    new_geo_coord_multipoint()
  } else if(inherits(x, "geo_coord_multipoint") && inherits(y, "geo_coord_point")) {
    new_geo_coord_multipoint()
  } else if (inherits(x, "geo_coord_linestring") && inherits(y, "geo_coord_multilinestring")) {
    new_geo_coord_multilinestring()
  } else if(inherits(x, "geo_coord_multilinestring") && inherits(y, "geo_coord_linestring")) {
    new_geo_coord_multilinestring()
  } else if (inherits(x, "geo_coord_polygon") && inherits(y, "geo_coord_multipolygon")) {
    new_geo_coord_multipolygon()
  } else if(inherits(x, "geo_coord_multipolygon") && inherits(y, "geo_coord_polygon")) {
    new_geo_coord_multipolygon()
  } else {
    NextMethod()
  }
}

#' @method vec_ptype2.geo_coord data.frame
#' @export
vec_ptype2.geo_coord.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  as.data.frame(geo_get_ptype_df(x))
}

#' @method vec_ptype2.geo_coord tbl_df
#' @export
vec_ptype2.geo_coord.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  geo_get_ptype_df(x)
}

#' @method vec_ptype2.data.frame geo_coord
#' @export
vec_ptype2.data.frame.geo_coord <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  as.data.frame(geo_get_ptype_df(y))
}

geo_get_ptype_df <- function(geo_coord) {
  ptype_default <- tibble(
    x = double(),
    y = double(),
    xy = geo_xy(),
    feature = integer(),
    part = integer(),
    piece = integer(),
    xmin = double(),
    ymin = double(),
    xmax = double(),
    ymax = double(),
    start = geo_xy(),
    end = geo_xy()
  )
  ptype_default[names(vec_data(geo_coord))]
}

#' @export
as_tibble.geo_coord <- function(x, ...) {
  as_tibble(vec_data(x))
}

#' @export
as.data.frame.geo_coord <- function(x, ...) {
  as.data.frame(as_tibble(x))
}

#' @method vec_cast geo_coord
#' @export
#' @export vec_cast.geo_coord
#' @rdname is_geo_coord
vec_cast.geo_coord <- function(x, to, ...) {
  UseMethod("vec_cast.geo_coord")
}

#' @method vec_cast.geo_coord default
#' @export
vec_cast.geo_coord.default <- function(x, to, ...) {
  vec_default_cast(x, to)
}

#' @method vec_cast.geo_coord geo_coord
#' @export
vec_cast.geo_coord.geo_coord <- function(x, to, ...) {
  # always pick 'multi' for common types
  # slightly easier to do it here rather than using a million double dispatch
  # methods
  if (inherits(x, "geo_coord_point") && inherits(to, "geo_coord_multipoint")) {
    new_geo_coord_multipoint(vec_data(x))
  } else if (inherits(x, "geo_coord_linestring") && inherits(to, "geo_coord_multilinestring")) {
    data <- vec_data(x)
    data$part <- rep_len(1L, vec_size(x))
    new_geo_coord_multilinestring(data)
  } else if (inherits(x, "geo_coord_polygon") && inherits(to, "geo_coord_multipolygon")) {
    data <- vec_data(x)
    data$part <- rep_len(1L, vec_size(x))
    new_geo_coord_multipolygon(data)
  } else if(identical(class(x), class(to))) {
    x
  } else {
    NextMethod()
  }
}

#' @method vec_cast.geo_coord data.frame
#' @export
vec_cast.geo_coord.data.frame <- function(x, to, ...) {
  vec_cast.geo_coord.list(x, to, ...)
}

#' @method vec_cast.data.frame geo_coord
#' @export
vec_cast.data.frame.geo_coord <- function(x, to, ...) {
  as.data.frame(as_tibble(vec_data(x)))
}

#' @method vec_cast.geo_coord list
#' @export
vec_cast.geo_coord.list <- function(x, to, ...) {
  to_class <- class(to)[1]
  if (!rlang::is_dictionaryish(x)) {
    abort(sprintf("Can't convert an unnamed list to <%s>", to_class))
  }
  constructor <- rlang::as_function(to_class, env = getNamespace("geom"))
  arg_names <- intersect(names(x), names(formals(constructor)))
  rlang::exec(constructor, !!!x[arg_names])
}

#' @method vec_cast.list geo_coord
#' @export
vec_cast.list.geo_coord <- function(x, to, ...) {
  vec_data(x)
}

#' @export
geo_restore.geo_coord <- function(to, x) {
  # the C++ data structure  for these is list() of geo_coords
  vec_c(!!!x)
}

#' @export
geo_size.geo_coord <- function(x) {
  if (vec_size(x) > 0) {
    max(field(x, "feature"))
  } else {
    0
  }
}
