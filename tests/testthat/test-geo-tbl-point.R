
test_that("geo_tbl_point() class works", {
  tbl_point <- geo_tbl_point(geo_xy(0, 1))
  expect_output(print(tbl_point), "geo_tbl_point")
  expect_output(print(tibble(tbl_point)), "tblpnt")
  expect_is(tbl_point, "geo_tbl_point")
  expect_is(tbl_point, "geo_tbl")

  expect_true(is_geo_tbl_point(tbl_point))
  expect_true(is_geo_tbl(tbl_point))
  expect_true(vec_is(tbl_point))

  # if we try to create a true multipoint, constructor should fail
  expect_error(geo_tbl_point(geo_xy(1:2, 1:2), feature = 1L), "Only one point")
})

test_that("geo_tbl_multipoint() class works", {
  tbl_point <- geo_tbl_multipoint(geo_xy(0, 1), feature = 1)
  expect_output(print(tbl_point), "geo_tbl_multipoint")
  expect_output(print(tibble(tbl_point)), "tblmpnt")
  expect_is(tbl_point, "geo_tbl_multipoint")
  expect_is(tbl_point, "geo_tbl")
  expect_true(is_geo_tbl_multipoint(tbl_point))
  expect_true(is_geo_tbl(tbl_point))
  expect_true(vec_is(tbl_point))

  # if we try to create a true multipoint, constructor should not fail
  expect_is(geo_tbl_multipoint(geo_xy(1:2, 1:2), feature = 1L), "geo_tbl_multipoint")
})

test_that("geo_tbl_point() c() and vec_c() works", {
  point <- geo_tbl_point(geo_xy(0:1, 1:2))

  point_in_tbl <- tibble(point)
  point_in_df <- as.data.frame(point_in_tbl)
  tbl_point <- as_tibble(point)
  df_point <- as.data.frame(tbl_point)

  expect_is(c(point, point), "geo_tbl_point")
  expect_length(c(point, point), 4)
  expect_is(vec_c(point, point), "geo_tbl_point")
  expect_length(vec_c(point, point), 4)
  expect_equal(nrow(vec_rbind(point_in_tbl, point_in_tbl)), 4)
  expect_is(vec_rbind(point_in_tbl, point_in_tbl)$point, "geo_tbl_point")

  # check vec_c() with tibble and data frame types
  expect_identical(c(point, tbl_point), vec_rbind(tbl_point, tbl_point))
  expect_identical(vec_c(point, tbl_point), vec_rbind(tbl_point, tbl_point))
  expect_identical(vec_c(tbl_point, point), vec_rbind(df_point, df_point)) # has to be a df output
  expect_identical(vec_c(point, df_point), vec_rbind(df_point, df_point))
  expect_identical(vec_c(df_point, point), vec_rbind(df_point, df_point))
})

test_that("geo_tbl_point() casting works", {
  point <- geo_tbl_point(geo_xy(0, 1))
  tbl_point <- as_tibble(point)
  df_point <- as.data.frame(point)

  expect_is(tbl_point, "tbl_df")
  expect_false(inherits(df_point, "tbl_df"))

  expect_identical(vec_cast(tbl_point, new_geo_tbl_point()), point)
  expect_identical(vec_cast(point, tibble()), as.data.frame(tbl_point))
  expect_identical(vec_cast(point, list()), vec_data(point))
  expect_identical(vec_cast(vec_data(tbl_point), new_geo_tbl_point()), point)

  expect_identical(as_geo_tbl_point(tbl_point), point)
  expect_identical(as_geo_tbl_point(df_point), point)
  expect_identical(as_geo_tbl_point(unclass(df_point)), point)
})
