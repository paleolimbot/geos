
test_that("geo_xy class works", {
  xy <- geo_xy(0, 1)
  expect_output(print(xy), "geo_xy")
  expect_is(xy, "geo_xy")
  expect_true(is_geo_xy(xy))
  expect_true(vec_is(xy))
})
