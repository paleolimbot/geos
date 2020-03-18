
test_that("geo_coord_polygon() class works", {
  tbl_polygon <- geo_coord_polygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_polygon), "geo_coord_polygon")
  expect_output(print(tibble(tbl_polygon)), "tblply")
  expect_is(tbl_polygon, "geo_coord_polygon")
  expect_true(is_geo_coord_polygon(tbl_polygon))
  expect_true(vec_is(tbl_polygon))
})

test_that("geo_coord_multipolygon() class works", {
  tbl_polygon <- geo_coord_multipolygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_polygon), "geo_coord_multipolygon")
  expect_output(print(tibble(tbl_polygon)), "tblmply")
  expect_is(tbl_polygon, "geo_coord_multipolygon")
  expect_true(is_geo_coord_multipolygon(tbl_polygon))
  expect_true(vec_is(tbl_polygon))
})


test_that("geo_coord_polygon() c() and vec_c() works", {
  polygon <- geo_coord_polygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))

  polygon_in_tbl <- tibble(polygon)
  polygon_in_df <- as.data.frame(polygon_in_tbl)
  tbl_polygon <- as_tibble(polygon)
  df_polygon <- as.data.frame(tbl_polygon)

  expect_is(c(polygon, polygon), "geo_coord_polygon")
  expect_length(c(polygon, polygon), 6)
  expect_is(vec_c(polygon, polygon), "geo_coord_polygon")
  expect_length(vec_c(polygon, polygon), 6)
  expect_equal(nrow(vec_rbind(polygon_in_tbl, polygon_in_tbl)), 6)
  expect_is(vec_rbind(polygon_in_tbl, polygon_in_tbl)$polygon, "geo_coord_polygon")

  # check vec_c() with tibble and data frame types
  expect_identical(c(polygon, tbl_polygon), vec_rbind(tbl_polygon, tbl_polygon))
  expect_identical(vec_c(polygon, tbl_polygon), vec_rbind(tbl_polygon, tbl_polygon))
  expect_identical(vec_c(tbl_polygon, polygon), vec_rbind(df_polygon, df_polygon))
  expect_identical(vec_c(polygon, df_polygon), vec_rbind(df_polygon, df_polygon))
  expect_identical(vec_c(df_polygon, polygon), vec_rbind(df_polygon, df_polygon))
})

test_that("geo_coord_polygon() casting works", {
  polygon <- geo_coord_polygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  tbl_polygon <- as_tibble(polygon)
  df_polygon <- as.data.frame(polygon)

  expect_is(tbl_polygon, "tbl_df")
  expect_false(inherits(df_polygon, "tbl_df"))

  expect_identical(vec_cast(tbl_polygon, new_geo_coord_polygon()), polygon)
  expect_identical(vec_cast(polygon, tibble()), as.data.frame(tbl_polygon))
  expect_identical(vec_cast(polygon, list()), vec_data(polygon))
  expect_identical(vec_cast(vec_data(tbl_polygon), new_geo_coord_polygon()), polygon)
  expect_error(
    vec_cast(unname(vec_data(tbl_polygon)), new_geo_coord_polygon()),
    "Can't convert an unnamed list"
  )

  expect_identical(as_geo_coord_polygon(tbl_polygon), polygon)
  expect_identical(as_geo_coord_polygon(df_polygon), polygon)
  expect_identical(as_geo_coord_polygon(unclass(df_polygon)), polygon)
})


test_that("geo_coord_multipolygon() c() and vec_c() works", {
  multipolygon <- geo_coord_multipolygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))

  multipolygon_in_tbl <- tibble(multipolygon)
  multipolygon_in_df <- as.data.frame(multipolygon_in_tbl)
  tbl_multipolygon <- as_tibble(multipolygon)
  df_multipolygon <- as.data.frame(tbl_multipolygon)

  expect_is(c(multipolygon, multipolygon), "geo_coord_multipolygon")
  expect_length(c(multipolygon, multipolygon), 6)
  expect_is(vec_c(multipolygon, multipolygon), "geo_coord_multipolygon")
  expect_length(vec_c(multipolygon, multipolygon), 6)
  expect_equal(nrow(vec_rbind(multipolygon_in_tbl, multipolygon_in_tbl)), 6)
  expect_is(
    vec_rbind(multipolygon_in_tbl, multipolygon_in_tbl)$multipolygon,
    "geo_coord_multipolygon"
  )

  # check vec_c() with tibble and data frame types
  expect_identical(
    c(multipolygon, tbl_multipolygon),
    vec_rbind(tbl_multipolygon, tbl_multipolygon)
  )
  expect_identical(
    vec_c(multipolygon, tbl_multipolygon),
    vec_rbind(tbl_multipolygon, tbl_multipolygon)
  )
  expect_identical(
    vec_c(tbl_multipolygon, multipolygon),
    vec_rbind(df_multipolygon, df_multipolygon)
  )
  expect_identical(
    vec_c(multipolygon, df_multipolygon),
    vec_rbind(df_multipolygon, df_multipolygon)
  )
  expect_identical(
    vec_c(df_multipolygon, multipolygon),
    vec_rbind(df_multipolygon, df_multipolygon)
  )
})

test_that("geo_coord_multipolygon() casting works", {
  multipolygon <- geo_coord_multipolygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  tbl_multipolygon <- as_tibble(multipolygon)
  df_multipolygon <- as.data.frame(multipolygon)

  expect_is(tbl_multipolygon, "tbl_df")
  expect_false(inherits(df_multipolygon, "tbl_df"))

  expect_identical(
    vec_cast(tbl_multipolygon, new_geo_coord_multipolygon()),
    multipolygon
  )
  expect_identical(vec_cast(multipolygon, tibble()), as.data.frame(tbl_multipolygon))
  expect_identical(vec_cast(multipolygon, list()), vec_data(multipolygon))
  expect_identical(vec_cast(vec_data(tbl_multipolygon), new_geo_coord_multipolygon()), multipolygon)
  expect_error(
    vec_cast(unname(vec_data(tbl_multipolygon)), new_geo_coord_multipolygon()),
    "Can't convert an unnamed list"
  )

  expect_identical(as_geo_coord_multipolygon(tbl_multipolygon), multipolygon)
  expect_identical(as_geo_coord_multipolygon(df_multipolygon), multipolygon)
  expect_identical(as_geo_coord_multipolygon(unclass(df_multipolygon)), multipolygon)
})

test_that("polygon is can be coerced to multipolygon", {
  multipolygon <- geo_coord_multipolygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  polygon <- geo_coord_polygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))

  expect_is(c(multipolygon, polygon), "geo_coord_multipolygon")
  expect_is(c(polygon, multipolygon), "geo_coord_multipolygon")
  expect_is(vec_c(multipolygon, polygon), "geo_coord_multipolygon")
  expect_is(vec_c(polygon, multipolygon), "geo_coord_multipolygon")
})
