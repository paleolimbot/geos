
test_that("geo_rect class works", {
  rect <- geo_rect(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  expect_output(print(rect), "geo_rect")
  expect_is(rect, "geo_rect")
  expect_true(is_geo_rect(rect))
  expect_true(vec_is(rect))
})


test_that("geo_rect c() works", {
  rect <- geo_rect(xmin = 0:5, ymin = 0:5, xmax = 1:6, ymax = 1:6)
  tbl <- tibble(rect)
  df <- as.data.frame(tbl)
  tbl_rect <- as_tibble(rect)
  df_rect <- as.data.frame(tbl_rect)

  expect_is(c(rect, rect), "geo_rect")
  expect_length(c(rect, rect), 12)
  expect_is(vec_c(rect, rect), "geo_rect")
  expect_length(vec_c(rect, rect), 12)
  expect_equal(nrow(vec_rbind(tbl, tbl)), 12)
  expect_is(vec_rbind(tibble(rect), tibble(rect))$rect, "geo_rect")

  # check vec_c() with tibble and data frame types
  expect_identical(c(rect, tbl_rect), vec_rbind(tbl_rect, tbl_rect))
  expect_identical(vec_c(rect, tbl_rect), vec_rbind(tbl_rect, tbl_rect))
  # because there's no vec_ptype2.tbl_df generic anywhere, this returns a df
  expect_identical(vec_c(tbl_rect, rect), vec_rbind(df_rect, df_rect))
  expect_identical(vec_c(rect, df_rect), vec_rbind(df_rect, df_rect))
  expect_identical(vec_c(df_rect, rect), vec_rbind(df_rect, df_rect))
})

test_that("geo_rect casting works", {
  rect <- geo_rect(xmin = 0:5, ymin = 0:5, xmax = 1:6, ymax = 1:6)

  expect_equal(as.data.frame(rect), data.frame(xmin = 0:5,  ymin = 0:5, xmax = 1:6, ymax = 1:6))
  expect_equal(tibble::as_tibble(rect), tibble(xmin = 0:5,  ymin = 0:5, xmax = 1:6, ymax = 1:6))

  expect_identical(dim(as.matrix(rect)), c(6L, 4L))

  expect_identical(vec_cast(rect, geo_rect()), rect)
  expect_identical(vec_cast(rect, list()), vec_data(rect))
  expect_identical(vec_cast(vec_data(rect), geo_rect()), rect)

  expect_identical(
    vec_cast(tibble(xmin = 0:5,  ymin = 0:5, xmax = 1:6, ymax = 1:6), geo_rect()),
    rect
  )

  expect_identical(
    as_geo_rect(tibble(xmin = 0:5,  ymin = 0:5, xmax = 1:6, ymax = 1:6)),
    rect
  )

  expect_identical(
    vec_c(
      geo_rect(xmin = 0:1, ymin = 0:1, xmax = 1:2, ymax = 1:2),
      tibble(xmin = 2:5, ymin = 2:5, xmax = 3:6, ymax = 3:6)
    ),
    tibble::as_tibble(vec_data(rect))
  )

  expect_identical(as_geo_rect(matrix(c(0:5, 0:5, 1:6, 1:6), ncol = 4)), rect)
  expect_identical(
    as_geo_rect(
      matrix(
        c(1:6, 1:6, 0:5, 0:5),
        ncol = 4,
        dimnames = list(NULL, c("ymax", "xmax", "xmin", "ymin"))
      )
    ),
    rect
  )
})
