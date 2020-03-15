
test_that("geo_buffer works", {
  point <- geo_wkt("POINT (0 0)")
  result <- geo_buffer(point, 1, quad_segs = 1)

  # TODO better test for geom buffer
  expect_length(result, 1)
  expect_is(result, "geo_wkt")
})
