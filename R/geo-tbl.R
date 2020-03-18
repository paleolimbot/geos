
#' Create geometries from data frames
#'
#' @param data A [tibble::tibble()] or data frame
#' @param col A column name for the [geo_xy()] object
#' @param x,y Column specifications for x and y coordinate
#'   columns, respectively. Use [dplyr::select()] syntax.
#' @param remove Use `remove = FALSE` to keep `x` and `y` as columns
#'   in the output.
#'
#' @return `data`, with a new column `col`
#' @export
#'
#' @examples
#' tbl <- tibble(x = 1, y = 2)
#' unite_xy(tbl, "xy", x, y, remove = FALSE)
#'
unite_xy <- function(data, col, x, y, remove = TRUE) {
  data <- tibble::as_tibble(data)

  x_col <- tidyselect::eval_select(enquo(x), data, strict = TRUE)
  y_col <- tidyselect::eval_select(enquo(y), data, strict = TRUE)
  stopifnot(
    length(x_col) == 1,
    length(y_col) == 1,
    length(col) ==  1, is.character(col)
  )

  xy <- geo_xy(x = data[[x_col]], y = data[[y_col]])

  insert_column(data, col, xy, c(x_col, y_col), remove)
}

#' @rdname unite_xy
#' @export
separate_xy <- function(data, col, into = c("x", "y"), remove = TRUE) {
  data <- tibble::as_tibble(data)

  xy_col <- tidyselect::eval_select(enquo(col), data, strict = TRUE)
  stopifnot(
    length(xy_col) == 1,
    length(into) == 2,
    is.character(into)
  )

  xy <- data[[xy_col]]
  stopifnot(is_geo_xy(xy))

  x <- field(xy, "x")
  y <- field(xy, "y")

  # until insert_column properly handles col as a tibble...
  data <- insert_column(data, into[2], y, xy_col, remove = remove)
  insert_column(data, into[1], x, xy_col, remove = FALSE)
}

insert_column <- function(data, col, value, source_cols, remove) {
  source_cols <- sort(source_cols)

  if (remove) {
    data[[source_cols[1]]] = value
    names(data)[source_cols[1]] <- col
    if (length(source_cols) > 1) {
      data <- data[-source_cols[-1]]
    }

    data
  } else {
    before <- source_cols[1]
    vec_cbind(
      data[seq_len(before - 1)],
      tibble(!!col := value),
      data[before:ncol(data)]
    )
  }
}
