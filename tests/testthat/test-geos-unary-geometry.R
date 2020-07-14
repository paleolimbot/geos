
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
      geos_unary_union(c("POLYGON ((1 0, 0 0, 0.5 0.5, 1 0))", NA))
    ),
    c("POLYGON ((1 0, 0 0, 0.5 0.5, 1 0))", NA)
  )

  expect_error(
    geos_write_wkt(
      geos_coverage_union(c("MULTIPOLYGON (((0 0, 1 0, 0.5 0.5, 0 0)), ((0 0, 1 0, 0.5 0.5, 0 0)))", NA))
    ),
    "cannot process overlapping inputs"
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

test_that("geos_buffer works", {
  expect_identical(geos_buffer(NA_character_, 1), geos_read_wkt(NA_character_))
  expect_identical(geos_buffer("POINT (0 0)", NA), geos_read_wkt(NA_character_))

  expect_equal(geos_xmin(geos_buffer("POINT (0 0)", 1)), -1)
  expect_equal(geos_ymin(geos_buffer("POINT (0 0)", 1)), -1)
  expect_equal(geos_xmax(geos_buffer("POINT (0 0)", 1)), 1)
  expect_equal(geos_ymax(geos_buffer("POINT (0 0)", 1)), 1)
})

test_that("geos_buffer errors with bad params", {
  expect_error(geos_buffer("POINT (0 0)", 1, params = NULL), "must be created using")

  good_params <- geos_buffer_params()

  params <- good_params
  params$end_cap_style = 10L
  expect_error(geos_buffer("POINT (0 0)", 1, params = params), "Invalid buffer endCap")

  params <- good_params
  params$join_style = 10L
  expect_error(geos_buffer("POINT (0 0)", 1, params = params), "Invalid buffer join")

  expect_error(geos_buffer("POINT (0 0)", Inf), "encountered NaN/Inf")
})
