
test_that("make point works", {
  expect_true(geos_is_empty(geos_make_point(NA, NA)))
  expect_identical(geos_write_wkt(geos_make_point(1, 2)), "POINT (1 2)")
  expect_identical(geos_write_wkt(geos_make_point(1, 2, 3)), "POINT Z (1 2 3)")
})
