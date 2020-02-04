
test_that("geo_tbl_point() class works", {
  tbl_point <- geo_tbl_point(geo_xy(0, 1))
  expect_output(print(tbl_point), "geo_tbl_point")
  expect_output(print(tibble(tbl_point)), "tblpnt")
  expect_is(tbl_point, "geo_tbl_point")
  expect_true(is_geo_tbl_point(tbl_point))
  expect_true(vec_is(tbl_point))

  # if we try to create a true multipoint, constructor should fail
  expect_error(geo_tbl_point(geo_xy(1:2, 1:2), feature = 1L), "Only one point")
})

test_that("geo_tbl_multipoint() class works", {
  tbl_point <- geo_tbl_multipoint(geo_xy(0, 1), feature = 1)
  expect_output(print(tbl_point), "geo_tbl_multipoint")
  expect_output(print(tibble(tbl_point)), "tblmpnt")
  expect_is(tbl_point, "geo_tbl_multipoint")
  expect_true(is_geo_tbl_multipoint(tbl_point))
  expect_true(vec_is(tbl_point))

  # if we try to create a true multipoint, constructor should not fail
  expect_is(geo_tbl_multipoint(geo_xy(1:2, 1:2), feature = 1L), "geo_tbl_multipoint")
})
