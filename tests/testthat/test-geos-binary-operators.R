
test_that("operators work", {
  poly1 <- geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
  poly2 <- geo_wkt("POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))")

  expect_identical(
    geos_intersection(poly1, poly2, to = geo_rect()),
    geo_rect(5, 5, 10, 10)
  )

  expect_identical(
    geos_difference(poly1, poly2, to = geo_rect()),
    geo_rect(0, 0, 10, 10)
  )

  expect_identical(
    geos_sym_difference(poly1, poly2, to = geo_rect()),
    geo_rect(0, 0, 15, 15)
  )

  expect_identical(
    geos_union(poly1, poly2, to = geo_rect()),
    geo_rect(0, 0, 15, 15)
  )

  collection <- geo_wkt("
    GEOMETRYCOLLECTION (
      POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0)),
      POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))
    )
  ")

  expect_identical(
    geos_unary_union(collection),
    geos_union(poly1, poly2)
  )

  clip <- geo_wkt("POLYGON ((-1 -1, -1 8, 8 8, 8 -1, -1 -1))")

  expect_true(
    geos_equals(
      geos_clip_by_rect(poly1, geo_rect(-1, -1, 8, 8)),
      geos_intersection(poly1, clip)
    )
  )
})

test_that("operators recycle geometry vectors", {
  result1 <- geos_intersection(
    geo_wkt("POINT (5 5)"),
    rep(geo_wkt("POLYGON ((5 5, 5 15, 10 15, 15 5, 5 5))"), 5)
  )

  result2 <- geos_intersection(
    rep(geo_wkt("POLYGON ((5 5, 5 15, 10 15, 15 5, 5 5))"), 5),
    geo_wkt("POINT (5 5)")
  )

  expect_identical(result1, result2)

  # the "zero length when anything is zero" behaviour mimics tibble::tibble()
  expect_identical(geos_intersection(geo_wkt("POINT (5 5)"), geo_wkt()), geo_wkt())
  expect_identical(geos_intersection(geo_wkt(), geo_wkt("POINT (5 5)")), geo_wkt())

  # also check when both are != 1
  result3 <- geos_intersection(
    rep(geo_wkt("POINT (5 5)"), 5),
    rep(geo_wkt("POLYGON ((5 5, 5 15, 10 15, 15 5, 5 5))"), 5)
  )

  expect_identical(result3, result2)

  # check bad lengths
  expect_error(
    geos_intersection(
      rep(geo_wkt("POINT (5 5)"), 3),
      rep(geo_wkt("POLYGON ((5 5, 5 15, 10 15, 15 5, 5 5))"), 5)
    ),
    "incompatible lengths"
  )
})

test_that("coverage union works", {
  skip_if_not(geos_version() >= "3.8.0")

  # disjoint polygons
  collection2 <- geo_wkt("
    GEOMETRYCOLLECTION (
      POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0)),
      POLYGON ((11 11, 11 15, 15 15, 15 11, 11 11))
    )
  ")

  expect_true(
    geos_equals(
      geos_coverage_union(collection2),
      geos_unary_union(collection2)
    )
  )
})
