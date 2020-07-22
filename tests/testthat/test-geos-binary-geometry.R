
test_that("binary operators work", {
  poly1 <- "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
  poly2 <- c(NA, "POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))")

  expect_identical(
    geos_area(geos_intersection(poly1, poly2)),
    c(NA, 25)
  )

  expect_identical(
    geos_area(geos_difference(poly1, poly2)),
    c(NA, 100 - 25)
  )

  expect_identical(
    geos_area(geos_sym_difference(poly1, poly2)),
    c(NA, 100 * 2 - 50)
  )

  expect_identical(
    geos_area(geos_union(poly1, poly2)),
    c(NA, 100 * 2 - 25)
  )

  collection <- "
    GEOMETRYCOLLECTION (
      POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0)),
      POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))
    )
  "

  expect_identical(
    geos_equals(
      geos_unary_union(c(NA, collection)),
      geos_union(poly1, poly2)
    ),
    c(NA, TRUE)
  )
})

test_that("snap works", {
  poly1 <- "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
  line <- c(NA, "LINESTRING (11 0, 11 10)")

  expect_identical(
    geos_equals(
      geos_snap(poly1, line),
      poly1
    ),
    c(NA, TRUE)
  )

  expect_identical(
    geos_equals(
      geos_snap(poly1, line, tolerance = 2),
      "POLYGON ((0 0, 0 10, 11 10, 11 0, 0 0))"
    ),
    c(NA, TRUE)
  )
})

