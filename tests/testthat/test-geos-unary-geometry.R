
test_that("transformers work", {
  expect_identical(
    geos_write_wkt(geos_centroid(c("POINT (0 1)", "LINESTRING (0 0, 1 1)", NA))),
    c("POINT (0 1)", "POINT (0.5 0.5)", NA)
  )

  expect_identical(
    geos_write_wkt(geos_boundary(c("POINT (0 1)", "LINESTRING (0 0, 1 1)", NA))),
    c("GEOMETRYCOLLECTION EMPTY", "MULTIPOINT (0 0, 1 1)", NA)
  )
  expect_error(geos_boundary("GEOMETRYCOLLECTION (POINT (0 1))"), "Operation not supported")

  expect_identical(
    geos_write_wkt(
      geos_minimum_width(c("POLYGON ((0 0, 1 0, 0 1, 0 0))", NA))
    ),
    c("LINESTRING (0.5 0.5, 0 0)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_minimum_clearance_line(c("POLYGON ((0 0, 10 0, 10 10, 3 5, 0 10, 0 0))", NA))
    ),
    c("LINESTRING (3 5, 0 5)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_minimum_rotated_rectangle(c("POLYGON ((0 0, 1 0, 0.5 0.5, 0 0))", NA))
    ),
    c("POLYGON ((1 0, 1 0.5, 0 0.5, 0 0, 1 0))", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_unary_union(c("MULTIPOLYGON (((0 0, 1 0, 0.5 0.5, 0 0)), ((0 0, 1 0, 0.5 0.5, 0 0)))", NA))
    ),
    c("POLYGON ((1 0, 0 0, 0.5 0.5, 1 0))", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_point_on_surface(c("POINT (0 1)", NA))
    ),
    c("POINT (0 1)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_node(c("POLYGON ((0 0, 1 0, 0 1, 0 0))", NA))
    ),
    c("MULTILINESTRING ((0 0, 1 0, 0 1, 0 0))", NA)
  )

  expect_identical(
    geos_num_geometries(
      geos_make_valid(c("POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))", NA))
    ),
    c(2L, NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_unique_points(c("POLYGON ((0 0, 1 0, 0 1, 0 0))", NA))
    ),
    c("MULTIPOINT (0 0, 1 0, 0 1)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_reverse(c("LINESTRING (0 0, 1 1)", NA))
    ),
    c("LINESTRING (1 1, 0 0)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_merge_lines(c("MULTILINESTRING ((0 0, 0.5 0.5, 2 2), (0.5 0.5, 2 2))", NA))
    ),
    c("LINESTRING (0 0, 0.5 0.5, 2 2, 0.5 0.5)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_build_area(c("LINESTRING (0 0, 1 0, 0 1, 0 0)", NA))
    ),
    c("POLYGON ((0 0, 0 1, 1 0, 0 0))", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_envelope(c("LINESTRING (0 0, 1 2)", NA))
    ),
    c("POLYGON ((0 0, 1 0, 1 2, 0 2, 0 0))", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_convex_hull(c("MULTIPOINT (0 0, 1 0, 0 2, 0 0)", NA))
    ),
    c("POLYGON ((0 0, 0 2, 1 0, 0 0))", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_point_start(c("LINESTRING (0 0, 1 2)", NA))
    ),
    c("POINT (0 0)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_point_end(c("LINESTRING (0 0, 1 2)", NA))
    ),
    c("POINT (1 2)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_clone(c("MULTIPOINT (0 0, 1 0, 0 2, 0 0)", NA))
    ),
    c("MULTIPOINT (0 0, 1 0, 0 2, 0 0)", NA)
  )
})

test_that("transformers with atomic param work", {
  expect_identical(
    geos_write_wkt(geos_interpolate(c("LINESTRING (0 0, 0 10, 10 10)", NA), 11)),
    c("POINT (1 10)", NA)
  )
  expect_error(geos_interpolate("POINT (0 1)", 1), "only supports lineal")

  expect_identical(
    geos_write_wkt(
      geos_interpolate_normalized(c("LINESTRING (0 0, 0 10, 10 10)", NA), c(11 / 20))
    ),
    c("POINT (1 10)", NA)
  )
  expect_error(geos_interpolate_normalized("POINT (0 1)", 1), "only works with LineString")

  expect_identical(
    geos_write_wkt(
      geos_point_n(c("LINESTRING (0 0, 1 2)", NA), 1)
    ),
    c("POINT (0 0)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_point_n(c("LINESTRING (0 0, 1 2)", NA), 2)
    ),
    c("POINT (1 2)", NA)
  )

  expect_identical(
    geos_write_wkt(
      geos_simplify("MULTILINESTRING ((0 0, 0 1, 0 2), (0.1 0, 0.1 1, 0.1 2))", 0.5)
    ),
    "MULTILINESTRING ((0 0, 0 2), (0.1 0, 0.1 2))"
  )

  expect_identical(
    geos_write_wkt(
      geos_simplify_preserve_topology("MULTILINESTRING ((0 0, 0 1, 0 2), (0.1 0, 0.1 1, 0.1 2))", 0.5)
    ),
    "MULTILINESTRING ((0 0, 0 2), (0.1 0, 0.1 2))"
  )


})

