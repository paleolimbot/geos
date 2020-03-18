
test_that("geo_coord_linestring() class works", {
  tbl_linestring <- geo_coord_linestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_linestring), "geo_coord_linestring")
  expect_output(print(tibble(tbl_linestring)), "tblls")
  expect_is(tbl_linestring, "geo_coord_linestring")
  expect_true(is_geo_coord_linestring(tbl_linestring))
  expect_true(vec_is(tbl_linestring))
})

test_that("geo_coord_multilinestring() class works", {
  tbl_linestring <- geo_coord_multilinestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_linestring), "geo_coord_multilinestring")
  expect_output(print(tibble(tbl_linestring)), "tblmls")
  expect_is(tbl_linestring, "geo_coord_multilinestring")
  expect_true(is_geo_coord_multilinestring(tbl_linestring))
  expect_true(vec_is(tbl_linestring))
})


test_that("geo_coord_linestring() c() and vec_c() works", {
  linestring <- geo_coord_linestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))

  linestring_in_tbl <- tibble(linestring)
  linestring_in_df <- as.data.frame(linestring_in_tbl)
  tbl_linestring <- as_tibble(linestring)
  df_linestring <- as.data.frame(tbl_linestring)

  expect_is(c(linestring, linestring), "geo_coord_linestring")
  expect_length(c(linestring, linestring), 6)
  expect_is(vec_c(linestring, linestring), "geo_coord_linestring")
  expect_length(vec_c(linestring, linestring), 6)
  expect_equal(nrow(vec_rbind(linestring_in_tbl, linestring_in_tbl)), 6)
  expect_is(vec_rbind(linestring_in_tbl, linestring_in_tbl)$linestring, "geo_coord_linestring")

  # check vec_c() with tibble and data frame types
  expect_identical(c(linestring, tbl_linestring), vec_rbind(tbl_linestring, tbl_linestring))
  expect_identical(vec_c(linestring, tbl_linestring), vec_rbind(tbl_linestring, tbl_linestring))
  expect_identical(vec_c(tbl_linestring, linestring), vec_rbind(df_linestring, df_linestring))
  expect_identical(vec_c(linestring, df_linestring), vec_rbind(df_linestring, df_linestring))
  expect_identical(vec_c(df_linestring, linestring), vec_rbind(df_linestring, df_linestring))
})

test_that("geo_coord_linestring() casting works", {
  linestring <- geo_coord_linestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  tbl_linestring <- as_tibble(linestring)
  df_linestring <- as.data.frame(linestring)

  expect_is(tbl_linestring, "tbl_df")
  expect_false(inherits(df_linestring, "tbl_df"))

  expect_identical(vec_cast(tbl_linestring, new_geo_coord_linestring()), linestring)
  expect_identical(vec_cast(linestring, tibble()), as.data.frame(tbl_linestring))
  expect_identical(vec_cast(linestring, list()), vec_data(linestring))
  expect_identical(vec_cast(vec_data(tbl_linestring), new_geo_coord_linestring()), linestring)
  expect_error(
    vec_cast(unname(vec_data(tbl_linestring)), new_geo_coord_linestring()),
    "Can't convert an unnamed list"
  )

  expect_identical(as_geo_coord_linestring(tbl_linestring), linestring)
  expect_identical(as_geo_coord_linestring(df_linestring), linestring)
  expect_identical(as_geo_coord_linestring(unclass(df_linestring)), linestring)
})


test_that("geo_coord_multilinestring() c() and vec_c() works", {
  multilinestring <- geo_coord_multilinestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))

  multilinestring_in_tbl <- tibble(multilinestring)
  multilinestring_in_df <- as.data.frame(multilinestring_in_tbl)
  tbl_multilinestring <- as_tibble(multilinestring)
  df_multilinestring <- as.data.frame(tbl_multilinestring)

  expect_is(c(multilinestring, multilinestring), "geo_coord_multilinestring")
  expect_length(c(multilinestring, multilinestring), 6)
  expect_is(vec_c(multilinestring, multilinestring), "geo_coord_multilinestring")
  expect_length(vec_c(multilinestring, multilinestring), 6)
  expect_equal(nrow(vec_rbind(multilinestring_in_tbl, multilinestring_in_tbl)), 6)
  expect_is(
    vec_rbind(multilinestring_in_tbl, multilinestring_in_tbl)$multilinestring,
    "geo_coord_multilinestring"
  )

  # check vec_c() with tibble and data frame types
  expect_identical(
    c(multilinestring, tbl_multilinestring),
    vec_rbind(tbl_multilinestring, tbl_multilinestring)
  )
  expect_identical(
    vec_c(multilinestring, tbl_multilinestring),
    vec_rbind(tbl_multilinestring, tbl_multilinestring)
  )
  expect_identical(
    vec_c(tbl_multilinestring, multilinestring),
    vec_rbind(df_multilinestring, df_multilinestring)
  )
  expect_identical(
    vec_c(multilinestring, df_multilinestring),
    vec_rbind(df_multilinestring, df_multilinestring)
  )
  expect_identical(
    vec_c(df_multilinestring, multilinestring),
    vec_rbind(df_multilinestring, df_multilinestring)
  )
})

test_that("geo_coord_multilinestring() casting works", {
  multilinestring <- geo_coord_multilinestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  tbl_multilinestring <- as_tibble(multilinestring)
  df_multilinestring <- as.data.frame(multilinestring)

  expect_is(tbl_multilinestring, "tbl_df")
  expect_false(inherits(df_multilinestring, "tbl_df"))

  expect_identical(
    vec_cast(tbl_multilinestring, new_geo_coord_multilinestring()),
    multilinestring
  )
  expect_identical(vec_cast(multilinestring, tibble()), as.data.frame(tbl_multilinestring))
  expect_identical(vec_cast(multilinestring, list()), vec_data(multilinestring))
  expect_identical(vec_cast(vec_data(tbl_multilinestring), new_geo_coord_multilinestring()), multilinestring)
  expect_error(
    vec_cast(unname(vec_data(tbl_multilinestring)), new_geo_coord_multilinestring()),
    "Can't convert an unnamed list"
  )

  expect_identical(as_geo_coord_multilinestring(tbl_multilinestring), multilinestring)
  expect_identical(as_geo_coord_multilinestring(df_multilinestring), multilinestring)
  expect_identical(as_geo_coord_multilinestring(unclass(df_multilinestring)), multilinestring)
})

test_that("linestring is can be coerced to multilinestring", {
  multilinestring <- geo_coord_multilinestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  linestring <- geo_coord_linestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))

  expect_is(c(multilinestring, linestring), "geo_coord_multilinestring")
  expect_is(c(linestring, multilinestring), "geo_coord_multilinestring")
  expect_is(vec_c(multilinestring, linestring), "geo_coord_multilinestring")
  expect_is(vec_c(linestring, multilinestring), "geo_coord_multilinestring")
})
