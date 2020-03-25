
test_that("area, length, and distance functions work", {
  poly <- geo_wkt("POLYGON ((0 0, 10 0, 0 10, 0 0))")
  expect_equal(geos_area(poly), 50)
  expect_equal(geos_length(poly), 10 + 10 + sqrt(200))
  expect_equal(geos_distance(poly, geo_wkt("POINT (10 10)")), sqrt(25 + 25))
})
