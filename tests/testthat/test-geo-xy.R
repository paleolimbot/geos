
test_that("geo_xy class works", {
  xy <- geo_xy(0, 1)
  expect_output(print(xy), "geo_xy")
  expect_is(xy, "geo_xy")
  expect_true(is_geo_xy(xy))
  expect_true(vec_is(xy))
})

test_that("geo_xy c() works", {
  xy <- geo_xy(0:5, 1:6)
  tbl <- tibble(xy)
  df <- as.data.frame(tbl)
  tbl_xy <- as_tibble(xy)
  df_xy <- as.data.frame(tbl_xy)

  expect_is(c(xy, xy), "geo_xy")
  expect_length(c(xy, xy), 12)
  expect_is(vec_c(xy, xy), "geo_xy")
  expect_length(vec_c(xy, xy), 12)
  expect_equal(nrow(vec_rbind(tbl, tbl)), 12)
  expect_is(vec_rbind(tibble(xy), tibble(xy))$xy, "geo_xy")

  # check vec_c() with tibble and data frame types
  expect_identical(c(xy, tbl_xy), vec_rbind(tbl_xy, tbl_xy))
  expect_identical(vec_c(xy, tbl_xy), vec_rbind(tbl_xy, tbl_xy))
  # because there's no vec_ptype2.tbl_df generic anywhere, this returns a df
  expect_identical(vec_c(tbl_xy, xy), vec_rbind(df_xy, df_xy))
  expect_identical(vec_c(xy, df_xy), vec_rbind(df_xy, df_xy))
  expect_identical(vec_c(df_xy, xy), vec_rbind(df_xy, df_xy))
})

test_that("geo_xy casting works", {
  xy <- geo_xy(0:5, 1:6)

  expect_equal(as.data.frame(xy), data.frame(x = 0:5, y = 1:6))
  expect_equal(tibble::as_tibble(xy), tibble(x = 0:5, y = 1:6))

  expect_identical(dim(as.matrix(xy)), c(6L, 2L))

  expect_identical(vec_cast(xy, geo_xy()), xy)
  expect_identical(vec_cast(xy, list()), vec_data(xy))
  expect_identical(vec_cast(vec_data(xy), geo_xy()), xy)

  expect_identical(
    vec_cast(tibble(x = 0:5, y = 1:6), geo_xy()),
    xy
  )

  expect_identical(
    as_geo_xy(tibble(x = 0:5, y = 1:6)),
    xy
  )

  expect_identical(
    vec_c(geo_xy(0:1, 1:2), tibble(x = 2:5, y = 3:6)),
    tibble::as_tibble(vec_data(xy))
  )

  expect_identical(as_geo_xy(matrix(c(0:5, 1:6), ncol = 2)), xy)
  expect_identical(
    as_geo_xy(
      matrix(c(1:6, 0:5), ncol = 2, dimnames = list(NULL, c("y", "x")))
    ),
    xy
  )
})
