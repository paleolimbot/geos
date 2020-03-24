
test_that("linear referencing works", {
  line <- geo_wkt("LINESTRING (0 0, 0 10)")
  points <- geos_interpolate(line, 0:10)
  expect_equal(geos_project(line, points), 0:10)

  distances <- geos_project_normalized(line, points)
  expect_equal(geos_interpolate_normalized(line, distances), points)
})

test_that("vectorization works for distances", {
  line <- geo_wkt("LINESTRING (0 0, 0 10)")
  expect_length(geos_interpolate(line, 1:10), 10)
  expect_length(geos_interpolate(rep(line, 10), 1:10), 10)
  expect_length(geos_interpolate(rep(line, 10), 1), 10)
})
