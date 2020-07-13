
test_that("distance functions work", {
  expect_identical(geos_distance(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
  expect_identical(geos_distance_indexed(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
  expect_identical(geos_distance_hausdorff(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
  expect_identical(geos_distance_frechet(c("POINT (0 0)", NA), "POINT (0 10)"), c(NaN, NA))
  expect_identical(geos_distance_frechet(c("POINT (0 0)", NA), "LINESTRING (0 10, 0 20)"), c(20, NA))
})

test_that("binary predicates work", {
  # check NA handling
  expect_identical(geos_disjoint(c("POINT EMPTY", NA), "POINT (0 0)"), c(TRUE, NA))

  # don't know how to get any of these to throw an exception

  expect_false(
    geos_disjoint(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_touches(
      "POINT (10 10)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_intersects(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_crosses(
      "LINESTRING (-1 -1, 6 6)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_within(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_contains(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_overlaps(
      "POLYGON ((1 1, 1 11, 11 11, 11 1, 1 1))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_equals(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_equals_exact(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_equals_exact(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      tolerance = 0.2
    )
  )

  expect_false(
    geos_equals_exact(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      tolerance = 0.05
    )
  )

  expect_identical(
    geos_equals_exact(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      tolerance = NA
    ),
    NA
  )

  expect_true(
    geos_covers(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_covered_by(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )
})

test_that("binary predicates work", {
  # check NA handling
  expect_identical(geos_prepared_disjoint(c("POINT EMPTY", NA), "POINT (0 0)"), c(TRUE, NA))

  # don't know how to get any of these to throw an exception

  expect_false(
    geos_prepared_disjoint(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_prepared_touches(
      "POINT (10 10)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_prepared_intersects(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_prepared_crosses(
      "LINESTRING (-1 -1, 6 6)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_prepared_within(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_prepared_contains(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_prepared_contains_properly(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_prepared_overlaps(
      "POLYGON ((1 1, 1 11, 11 11, 11 1, 1 1))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_prepared_covers(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_prepared_covered_by(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )
})
