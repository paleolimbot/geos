
test_that("geo_tbl_linestring() class works", {
  tbl_linestring <- geo_tbl_linestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_linestring), "geo_tbl_linestring")
  expect_output(print(tibble(tbl_linestring)), "tblls")
  expect_is(tbl_linestring, "geo_tbl_linestring")
  expect_true(is_geo_tbl_linestring(tbl_linestring))
  expect_true(vec_is(tbl_linestring))
})

test_that("geo_tbl_multilinestring() class works", {
  tbl_linestring <- geo_tbl_multilinestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))
  expect_output(print(tbl_linestring), "geo_tbl_multilinestring")
  expect_output(print(tibble(tbl_linestring)), "tblmls")
  expect_is(tbl_linestring, "geo_tbl_multilinestring")
  expect_true(is_geo_tbl_multilinestring(tbl_linestring))
  expect_true(vec_is(tbl_linestring))
})
