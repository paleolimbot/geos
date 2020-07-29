
test_that("ploting works", {
  geom <- as_geos_geometry("LINESTRING (0 0, 1 1)")
  expect_identical(plot(geom), geom)
})
