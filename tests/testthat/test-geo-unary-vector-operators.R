
test_that("unary predicates work", {
  expect_true(geos_is_empty(geo_wkt("POINT EMPTY")))
  expect_false(geos_is_empty(geo_wkt("POINT (30 10)")))

  # is self-intersecting
  expect_false(geos_is_simple(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0, 10 10)")))
  expect_true(geos_is_simple(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0)")))

  expect_true(geos_has_z(geo_wkt("POINT (10 10 1)")))
  expect_false(geos_has_z(geo_wkt("POINT (10 10)")))

  expect_true(geos_is_closed(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0)")))
  expect_false(geos_is_closed(geo_wkt("LINESTRING (0 0, 0 10, 10 0)")))
})
