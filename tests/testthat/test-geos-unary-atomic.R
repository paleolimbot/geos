
test_that("atomic extractors work", {
  expect_identical(
    geos_area(c("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", NA)),
    c(100, NA)
  )

  expect_identical(
    geos_length(c("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", NA)),
    c(40, NA)
  )

  expect_identical(geos_x(c("POINT Z (1 2 3)", NA)), c(1, NA))
  expect_identical(geos_y(c("POINT Z (1 2 3)", NA)), c(2, NA))
  expect_identical(geos_z(c("POINT Z (1 2 3)", NA)), c(3, NA))
  expect_error(geos_x("LINESTRING (0 1, 1 2)"), "Argument is not a Point")

  expect_identical(geos_xmin(c("LINESTRING (0 1, 2 3)", NA)), c(0, NA))
  expect_identical(geos_ymin(c("LINESTRING (0 1, 2 3)", NA)), c(1, NA))
  expect_identical(geos_xmax(c("LINESTRING (0 1, 2 3)", NA)), c(2, NA))
  expect_identical(geos_ymax(c("LINESTRING (0 1, 2 3)", NA)), c(3, NA))

  expect_identical(
    geos_minimum_clearance(c("POLYGON ((0 0, 10 0, 10 10, 3 5, 0 10, 0 0))", NA)),
    c(3, NA)
  )
})

test_that("atomic returners work", {
  expect_identical(geos_is_empty(c("POINT EMPTY", "POINT (0 1)", NA)), c(TRUE, FALSE, NA))
  expect_identical(
    geos_is_simple(c("LINESTRING (0 0, 1 1)", "LINESTRING (0 0, 1 1, 1 0, 0 1)", NA)),
    c(TRUE, FALSE, NA)
  )
  expect_identical(
    geos_is_ring(c("LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)", "POINT (0 1)", NA)),
    c(TRUE, FALSE, NA)
  )
  expect_identical(
    geos_is_closed(c("LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)", "LINESTRING (0 0, 1 1)", NA)),
    c(TRUE, FALSE, NA)
  )
  expect_error(geos_is_closed("POINT (0 1)"), "Argument is not a LineString")

  expect_identical(
    geos_is_valid(c("LINESTRING (0 0, 1 1)", "POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))", NA)),
    c(TRUE, FALSE, NA)
  )
})
