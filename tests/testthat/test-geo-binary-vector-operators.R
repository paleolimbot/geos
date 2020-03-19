
test_that("spatial predicates work", {

  expect_false(
    geo_disjoint(
      geo_wkt("POINT (5 5)"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )

  expect_true(
    geo_touches(
      geo_wkt("POINT (10 10)"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )

  expect_true(
    geo_intersects(
      geo_wkt("POINT (5 5)"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )

  expect_true(
    geo_crosses(
      geo_wkt("LINESTRING (-1 -1, 6 6)"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )

  expect_true(
    geo_within(
      geo_wkt("POINT (5 5)"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )

  expect_true(
    geo_contains(
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"),
      geo_wkt("POINT (5 5)")
    )
  )

  expect_true(
    geo_overlaps(
      geo_wkt("POLYGON ((1 1, 1 11, 11 11, 11 1, 1 1))"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )

  expect_true(
    geo_equals(
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )

  expect_true(
    geo_covers(
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"),
      geo_wkt("POINT (5 5)")
    )
  )

  expect_true(
    geo_covered_by(
      geo_wkt("POINT (5 5)"),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    )
  )
})

test_that("binary operators recycle geometry providers to a common length", {
  expect_length(
    geo_intersects(
      rep(geo_wkt("POINT (5 5)"), 5),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    ),
    5
  )

  expect_length(
    geo_intersects(
      rep(geo_wkt("POINT (5 5)"), 0),
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
    ),
    0
  )

  expect_length(
    geo_intersects(
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"),
      rep(geo_wkt("POINT (5 5)"), 5)
    ),
    5
  )

  expect_length(
    geo_intersects(
      geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"),
      rep(geo_wkt("POINT (5 5)"), 0)
    ),
    0
  )

  expect_length(
    geo_intersects(
      rep(geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"), 5),
      rep(geo_wkt("POINT (5 5)"), 5)
    ),
    5
  )
})
