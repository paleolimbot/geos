
test_that("geo_coord_point() class works", {
  tbl_point <- geo_coord_point(geo_xy(0, 1))
  expect_output(print(tbl_point), "geo_coord_point")
  expect_output(print(tibble(tbl_point)), "tblpnt")
  expect_is(tbl_point, "geo_coord_point")
  expect_is(tbl_point, "geo_coord")

  expect_true(is_geo_coord_point(tbl_point))
  expect_true(is_geo_coord(tbl_point))
  expect_true(vec_is(tbl_point))

  # if we try to create a true multipoint, constructor should fail
  expect_error(geo_coord_point(geo_xy(1:2, 1:2), feature = 1L), "Only one point")
})

test_that("geo_coord_multipoint() class works", {
  tbl_point <- geo_coord_multipoint(geo_xy(0, 1), feature = 1)
  expect_output(print(tbl_point), "geo_coord_multipoint")
  expect_output(print(tibble(tbl_point)), "tblmpnt")
  expect_is(tbl_point, "geo_coord_multipoint")
  expect_is(tbl_point, "geo_coord")
  expect_true(is_geo_coord_multipoint(tbl_point))
  expect_true(is_geo_coord(tbl_point))
  expect_true(vec_is(tbl_point))

  # if we try to create a true multipoint, constructor should not fail
  expect_is(geo_coord_multipoint(geo_xy(1:2, 1:2), feature = 1L), "geo_coord_multipoint")
})

test_that("geo_coord_point() c() and vec_c() works", {
  point <- geo_coord_point(geo_xy(0:1, 1:2))

  point_in_tbl <- tibble(point)
  point_in_df <- as.data.frame(point_in_tbl)
  tbl_point <- as_tibble(point)
  df_point <- as.data.frame(tbl_point)

  expect_is(c(point, point), "geo_coord_point")
  expect_length(c(point, point), 4)
  expect_is(vec_c(point, point), "geo_coord_point")
  expect_length(vec_c(point, point), 4)
  expect_equal(nrow(vec_rbind(point_in_tbl, point_in_tbl)), 4)
  expect_is(vec_rbind(point_in_tbl, point_in_tbl)$point, "geo_coord_point")

  # check vec_c() with tibble and data frame types
  expect_identical(c(point, tbl_point), vec_rbind(tbl_point, tbl_point))
  expect_identical(vec_c(point, tbl_point), vec_rbind(tbl_point, tbl_point))
  expect_identical(vec_c(tbl_point, point), vec_rbind(df_point, df_point)) # has to be a df output
  expect_identical(vec_c(point, df_point), vec_rbind(df_point, df_point))
  expect_identical(vec_c(df_point, point), vec_rbind(df_point, df_point))
})

test_that("geo_coord_point() casting works", {
  point <- geo_coord_point(geo_xy(0, 1))
  tbl_point <- as_tibble(point)
  df_point <- as.data.frame(point)

  expect_is(tbl_point, "tbl_df")
  expect_false(inherits(df_point, "tbl_df"))

  expect_identical(vec_cast(tbl_point, new_geo_coord_point()), point)
  expect_identical(vec_cast(point, tibble()), as.data.frame(tbl_point))
  expect_identical(vec_cast(point, list()), vec_data(point))
  expect_identical(vec_cast(vec_data(tbl_point), new_geo_coord_point()), point)
  expect_error(
    vec_cast(unname(vec_data(tbl_point)), new_geo_coord_point()),
    "Can't convert an unnamed list"
  )

  expect_identical(as_geo_coord_point(tbl_point), point)
  expect_identical(as_geo_coord_point(df_point), point)
  expect_identical(as_geo_coord_point(unclass(df_point)), point)
})


test_that("geo_coord_multipoint() c() and vec_c() works", {
  multipoint <- geo_coord_multipoint(geo_xy(0:1, 1:2))

  multipoint_in_tbl <- tibble(multipoint)
  multipoint_in_df <- as.data.frame(multipoint_in_tbl)
  tbl_multipoint <- as_tibble(multipoint)
  df_multipoint <- as.data.frame(tbl_multipoint)

  expect_is(c(multipoint, multipoint), "geo_coord_multipoint")
  expect_length(c(multipoint, multipoint), 4)
  expect_is(vec_c(multipoint, multipoint), "geo_coord_multipoint")
  expect_length(vec_c(multipoint, multipoint), 4)
  expect_equal(nrow(vec_rbind(multipoint_in_tbl, multipoint_in_tbl)), 4)
  expect_is(vec_rbind(multipoint_in_tbl, multipoint_in_tbl)$multipoint, "geo_coord_multipoint")

  # check vec_c() with tibble and data frame types
  expect_identical(c(multipoint, tbl_multipoint), vec_rbind(tbl_multipoint, tbl_multipoint))
  expect_identical(vec_c(multipoint, tbl_multipoint), vec_rbind(tbl_multipoint, tbl_multipoint))
  expect_identical(vec_c(tbl_multipoint, multipoint), vec_rbind(df_multipoint, df_multipoint))
  expect_identical(vec_c(multipoint, df_multipoint), vec_rbind(df_multipoint, df_multipoint))
  expect_identical(vec_c(df_multipoint, multipoint), vec_rbind(df_multipoint, df_multipoint))
})

test_that("geo_coord_multipoint() casting works", {
  multipoint <- geo_coord_multipoint(geo_xy(0, 1))
  tbl_multipoint <- as_tibble(multipoint)
  df_multipoint <- as.data.frame(multipoint)

  expect_is(tbl_multipoint, "tbl_df")
  expect_false(inherits(df_multipoint, "tbl_df"))

  expect_identical(vec_cast(tbl_multipoint, new_geo_coord_multipoint()), multipoint)
  expect_identical(vec_cast(multipoint, tibble()), as.data.frame(tbl_multipoint))
  expect_identical(vec_cast(multipoint, list()), vec_data(multipoint))
  expect_identical(vec_cast(vec_data(tbl_multipoint), new_geo_coord_multipoint()), multipoint)
  expect_error(
    vec_cast(unname(vec_data(tbl_multipoint)), new_geo_coord_multipoint()),
    "Can't convert an unnamed list"
  )

  expect_identical(as_geo_coord_multipoint(tbl_multipoint), multipoint)
  expect_identical(as_geo_coord_multipoint(df_multipoint), multipoint)
  expect_identical(as_geo_coord_multipoint(unclass(df_multipoint)), multipoint)
})

test_that("point is can be coerced to multipoint", {
  multipoint <- geo_coord_multipoint(geo_xy(0, 1))
  point <- geo_coord_point(geo_xy(2, 3))

  expect_is(c(multipoint, point), "geo_coord_multipoint")
  expect_is(c(point, multipoint), "geo_coord_multipoint")
  expect_is(vec_c(multipoint, point), "geo_coord_multipoint")
  expect_is(vec_c(point, multipoint), "geo_coord_multipoint")
})
