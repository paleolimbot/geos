
#' Create geometries from data frames
#'
#' @param data A [tibble::tibble()] or data frame
#' @param col A column name as a quoted string
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

  if (remove) {
    data[[x_col]] = xy
    names(data)[x_col] <- col
    data[-y_col]
  } else {
    before <- min(c(x_col, y_col))
    vec_cbind(
      data[seq_len(before - 1)],
      tibble(!!col := xy),
      data[before:ncol(data)]
    )
  }
}
