
test_that("pattern for erroring on mismatched CRSs works", {
  expect_error(
    geos_distance(
      as_geos_geometry("POINT (0 1)", crs = 1234),
      as_geos_geometry("POINT (0 1)", crs = 5678)
    ),
    "are not equal"
  )
})

test_that("distance functions work", {
  expect_identical(geos_distance(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
  expect_identical(geos_distance_indexed(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
  expect_identical(geos_distance_hausdorff(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
  expect_identical(
    geos_distance_hausdorff(c("POINT (0 0)", NA), "LINESTRING (0 10, 0 20)", densify = 0.1),
    c(20, NA)
  )

  if (geos_version() >= "3.10") {
    expect_identical(geos_distance_frechet(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
  } else {
    expect_identical(geos_distance_frechet(c("POINT (0 0)", NA), "POINT (0 10)"), c(NaN, NA))
  }

  expect_identical(geos_distance_frechet(c("POINT (0 0)", NA), "LINESTRING (0 10, 0 20)"), c(20, NA))
  expect_identical(
    geos_distance_frechet(c("POINT (0 0)", NA), "LINESTRING (0 10, 0 20)", densify = 0.1),
    c(20, NA)
  )
})

test_that("prepared distance function works", {
  skip_if_not(geos_version() >= "3.9.1")

  expect_identical(geos_prepared_distance(c("POINT (0 0)", NA), "POINT (0 10)"), c(10, NA))
})

test_that("within distance functions work", {
  skip_if_not(geos_version() >= "3.10.0")

  expect_identical(
    geos_is_within_distance(
      c("POINT (0 0)", "POINT (0 0)", NA),
      c("POINT (0 10)", "POINT (0 20)", "POINT (0 10)"),
      15
    ),
    c(TRUE, FALSE, NA)
  )

  expect_identical(
    geos_prepared_is_within_distance(
      c("POINT (0 0)", "POINT (0 0)", NA),
      c("POINT (0 10)", "POINT (0 20)", "POINT (0 10)"),
      15
    ),
    c(TRUE, FALSE, NA)
  )
})

test_that("linear referencing works", {
  expect_error(geos_project("POINT (0 0)", "POINT (0 0)"), "only supports lineal geometry")
  expect_identical(
    geos_project(c("LINESTRING (0 0, 0 10)", NA), "POINT (0 1)"),
    c(1, NA)
  )
  expect_identical(
    geos_project("LINESTRING (0 0, 0 10)", c("POINT (0 1)", "POINT EMPTY", NA)),
    c(1, NaN, NA)
  )

  expect_identical(
    geos_project_normalized("LINESTRING (0 0, 0 10)", c("POINT (0 1)", "POINT EMPTY", NA)),
    c(0.1, NaN, NA)
  )
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

test_that("prepared binary predicates re-use cached prepared geometry", {
  # this is hard to test from R but we can at least ensure that the line
  # gets hit in code coverage
  geom <- as_geos_geometry("POINT (5 5)")
  expect_true(
    geos_prepared_intersects(
      geom,
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  # the second time should return the same result but not
  # re-compute the prepared geometry
  expect_true(
    geos_prepared_intersects(
      geom,
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )
})

test_that("DE9IM functions work", {
  expect_identical(
    geos_relate_pattern_match("FF*FF****", c(NA, "FF*FF****", "FF*FF***F")),
    c(NA, TRUE, FALSE)
  )
  expect_error(geos_relate_pattern_match("fish", "fish"), "Unknown dimension symbol")

  expect_identical(geos_relate("POINT (0 0)", c(NA, "POINT (0 0)")), c(NA, "0FFFFFFF2"))
  expect_true(geos_relate_pattern("POINT (0 0)", "POINT (0 0)", "0FFFFFFF2"))
})

test_that("patttern maker works", {
  expect_identical(geos_relate_pattern_create(), strrep("*", 9))
  expect_identical(geos_relate_pattern_create(EE = 2), paste0(strrep("*", 8), 2))
  expect_identical(geos_relate_pattern_create(EE = NA), NA_character_)
  expect_identical(geos_relate_pattern_create(EE = character(0)), character(0))
  expect_error(geos_relate_pattern_create(3), "All pattern characters must be one of")
})
