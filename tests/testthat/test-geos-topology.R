
test_that("summarisers work", {

  poly <- geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
  poly_hole <- geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0), (4 4, 4 6, 6 6, 6 4, 4 4))")

  expect_identical(geos_centroid(poly), geos_point_on_surface(poly))
  expect_false(
    geos_equals(
      geos_point_on_surface(poly_hole),
      geos_centroid(poly)
    )
  )

  expect_true(
    geos_equals(
      geos_node(geo_wkt("MULTILINESTRING ((0 0, 10 10), (0 0, 10 10))")),
      geo_wkt("MULTILINESTRING ((0 0, 10 10))")
    )
  )

  expect_true(
    geos_equals(
      geos_boundary(poly),
      geo_wkt("LINESTRING (0 0, 0 10, 10 10, 10 0, 0 0)")
    )
  )

  expect_true(geos_equals(geos_envelope(poly), poly))
  expect_true(geos_equals(geos_convex_hull(poly), poly))
  expect_true(geos_equals(geos_minimum_rotated_rectangle(poly), poly))

  expect_equal(geos_length(geos_minimum_width(poly)), 10)
  expect_equal(geos_length(geos_minimum_clearance_line(poly)), 10)
  expect_equal(geos_minimum_clearance(poly), 10)

  line <- geo_wkt("LINESTRING (30 10, 10 30, 40 40)")
  expect_true(
    geos_equals(
      geos_convex_hull(line),
      geo_wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))")
    )
  )
})

test_that("summarisers in GEOS 3.8+ work",  {
  skip_if_not(geos_version() >= "3.8.0")

  poly <- geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")

  expect_true(
    geos_intersects(
      geos_minimum_bounding_circle(poly),
      poly
    )
  )

  expect_identical(
    geos_minimum_bounding_circle_center(poly),
    geos_centroid(poly)
  )

  expect_equal(geos_minimum_bounding_circle_radius(poly), sqrt(200) / 2)
})
