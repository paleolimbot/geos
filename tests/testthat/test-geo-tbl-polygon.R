
test_that("geo_tbl_polygon() class works", {
  tbl_polygon <- geo_tbl_polygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_polygon), "geo_tbl_polygon")
  expect_output(print(tibble(tbl_polygon)), "tblply")
  expect_is(tbl_polygon, "geo_tbl_polygon")
  expect_true(is_geo_tbl_polygon(tbl_polygon))
  expect_true(vec_is(tbl_polygon))
})

test_that("geo_tbl_multipolygon() class works", {
  tbl_polygon <- geo_tbl_multipolygon(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_polygon), "geo_tbl_multipolygon")
  expect_output(print(tibble(tbl_polygon)), "tblmply")
  expect_is(tbl_polygon, "geo_tbl_multipolygon")
  expect_true(is_geo_tbl_multipolygon(tbl_polygon))
  expect_true(vec_is(tbl_polygon))
})
