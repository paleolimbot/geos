
test_that("pattern for propagating crs works", {
  expect_identical(
    wk::wk_crs(geos_centroid(as_geos_geometry("POINT (0 1)", crs = 12345))),
    12345
  )
})

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
    geos_equals(
      geos_unary_union(c("MULTIPOLYGON (((0 0, 1 0, 0.5 0.5, 0 0)), ((0 0, 1 0, 0.5 0.5, 0 0)))", NA)),
      c("POLYGON ((1 0, 0 0, 0.5 0.5, 1 0))", NA)
    ),
    c(TRUE, NA)
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

  expect_identical(
    geos_srid(
      geos_set_srid(c(NA, "POINT (30 10)", "POINT (30 10)", "POINT (30 10)"), c(12, NA, 0, 12))
    ),
    c(NA, NA, 0L, 12L)
  )
})

test_that("geos_unary_union_prec() works", {
  if ((geos_version(runtime = TRUE) >= "3.9.1") && (geos_version(runtime = FALSE) >= "3.9.1")) {
    expect_identical(
      geos_equals(
        geos_unary_union_prec(
          c("MULTIPOLYGON (((0 0, 1 0, 0.5 0.5, 0 0)), ((0 0, 1 0, 0.5 0.5, 0 0)))", NA),
          0.1
        ),
        c("POLYGON ((1 0, 0 0, 0.5 0.5, 1 0))", NA)
      ),
      c(TRUE, NA)
    )
  } else if(geos_version(runtime = FALSE) >= "3.9.1") {
    expect_error(
      geos_unary_union_prec("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 0.01),
      "requires 'libgeos'"
    )
  } else {
    expect_error(
      geos_unary_union_prec("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 0.01),
      "built against 'libgeos'"
    )
  }
})

test_that("geos_maximum_inscribed_circle_spec() works", {
  if ((geos_version(runtime = TRUE) >= "3.9.1") && (geos_version(runtime = FALSE) >= "3.9.1")) {
    expect_equal(
      geos_length(
        geos_maximum_inscribed_circle_spec("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 0.01)
      ),
      0.5
    )

    expect_identical(
      geos_maximum_inscribed_crc("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 0.01),
      wk::crc(0.5, 0.5, 0.5)
    )

  } else if(geos_version(runtime = FALSE) >= "3.9.1") {
    expect_error(
      geos_maximum_inscribed_circle_spec("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 0.01),
      "requires 'libgeos'"
    )
  } else {
    expect_error(
      geos_maximum_inscribed_circle_spec("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", 0.01),
      "built against 'libgeos'"
    )
  }
})

test_that("transformers with atomic param work", {
  expect_identical(
    geos_write_wkt(geos_interpolate(c("LINESTRING (0 0, 0 10, 10 10)", NA), 11)),
    c("POINT (1 10)", NA)
  )
  expect_identical(geos_interpolate("LINESTRING (0 0, 1 1)", NA_real_), as_geos_geometry(NA_character_))
  expect_error(geos_interpolate("POINT (0 1)", 1), "only supports lineal")

  expect_identical(
    geos_write_wkt(
      geos_interpolate_normalized(c("LINESTRING (0 0, 0 10, 10 10)", NA), c(11 / 20))
    ),
    c("POINT (1 10)", NA)
  )
  expect_identical(
    geos_interpolate_normalized("LINESTRING (0 0, 1 1)", NA_real_),
    as_geos_geometry(NA_character_)
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
    geos_point_n("LINESTRING (0 0, 1 1)", NA_integer_),
    as_geos_geometry(NA_character_)
  )

  expect_identical(
    geos_write_wkt(
      geos_simplify("MULTILINESTRING ((0 0, 0 1, 0 2), (0.1 0, 0.1 1, 0.1 2))", 0.5)
    ),
    "MULTILINESTRING ((0 0, 0 2), (0.1 0, 0.1 2))"
  )
  expect_identical(
    geos_simplify("LINESTRING (0 0, 1 1)", NA_real_),
    as_geos_geometry(NA_character_)
  )

  expect_identical(
    geos_write_wkt(
      geos_simplify_preserve_topology("MULTILINESTRING ((0 0, 0 1, 0 2), (0.1 0, 0.1 1, 0.1 2))", 0.5)
    ),
    "MULTILINESTRING ((0 0, 0 2), (0.1 0, 0.1 2))"
  )
  expect_identical(
    geos_simplify_preserve_topology("LINESTRING (0 0, 1 1)", NA_real_),
    as_geos_geometry(NA_character_)
  )

  expect_identical(
    geos_write_wkt(geos_normalize(c(NA, "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"))),
    geos_write_wkt(geos_normalize(c(NA, "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")))
  )

  expect_identical(geos_normalize(NA_character_), geos_read_wkt(NA_character_))
})

test_that("set precision works", {
  expect_identical(
    geos_equals(
      geos_set_precision(
        c(NA, "POINT (0.1 1.1)", "POINT (0.1 1.1)", "POINT (0.1 1.1)"),
        c(0, NA, 0, 1)
      ),
      c(NA, NA, "POINT (0.1 1.1)", "POINT (0 1)")
    ),
    c(NA, NA, TRUE, TRUE)
  )

  if (geos_version() < "3.9.0") {
    expect_error(geos_set_precision("POINT (0 0)", -1), "IllegalArgumentException")
  }

  odd_snap_poly <-
    "POLYGON ((0 0, 0.9 0, 0 0.9, 0 0), (0.46 0.46, 0.3 0.46, 0.46 0.3, 0.46 0.46))"
  expect_false(
    geos_write_wkt(
      geos_set_precision(odd_snap_poly, 0.1, preserve_topology = T)
    ) ==
      geos_write_wkt(
        geos_set_precision(odd_snap_poly, 0.1, preserve_topology = F)
      )
  )

  odd_snap_collection <- "GEOMETRYCOLLECTION (LINESTRING (0 0, 0.04 0.04), POINT (10 10))"
  expect_equal(
    geos_num_geometries(geos_set_precision(odd_snap_collection, 0.1, keep_collapsed = FALSE)),
    1
  )
  expect_equal(
    geos_num_geometries(geos_set_precision(odd_snap_collection, 0.1, keep_collapsed = TRUE)),
    2
  )
})

test_that("bounding circle works", {
  circle <- geos_minimum_bounding_circle(c(NA, "LINESTRING (-1 -1, 1 1)"))
  expect_equal(geos_xmin(circle), c(NA, -sqrt(2)))
  expect_equal(geos_ymin(circle), c(NA, -sqrt(2)))
  expect_equal(geos_xmax(circle), c(NA, sqrt(2)))
  expect_equal(geos_ymax(circle), c(NA, sqrt(2)))

  expect_error(geos_minimum_bounding_circle("POINT (nan inf)"), "encountered NaN/Inf")
})

test_that("bounding crc works", {
  expect_identical(
    geos_minimum_bounding_crc(c(NA, "LINESTRING (-1 -1, 1 1)")),
    wk::crc(
      c(NA, 0),
      c(NA, 0),
      c(NA, sqrt(2))
    )
  )
})

test_that("clip by rect works", {
  line <- "LINESTRING (-1 5, 11 5)"
  expect_identical(
    geos_write_wkt(geos_clip_by_rect(c(NA, line), list(0, 0, 10, 10))),
    c(NA, "LINESTRING (0 5, 10 5)")
  )
  expect_identical(
    geos_write_wkt(geos_clip_by_rect(line, list(c(NA, 0), 0, 10, 10))),
    c(NA, "LINESTRING (0 5, 10 5)")
  )
  expect_identical(
    geos_write_wkt(geos_clip_by_rect(line, list(0, c(NA, 0), 10, 10))),
    c(NA, "LINESTRING (0 5, 10 5)")
  )
  expect_identical(
    geos_write_wkt(geos_clip_by_rect(line, list(0, 0, c(NA, 10), 10))),
    c(NA, "LINESTRING (0 5, 10 5)")
  )
  expect_identical(
    geos_write_wkt(geos_clip_by_rect(line, list(0, 0, 10, c(NA, 10)))),
    c(NA, "LINESTRING (0 5, 10 5)")
  )
  expect_identical(
    geos_write_wkt(geos_clip_by_rect(line, list(-Inf, -Inf, Inf, Inf))),
    line
  )
  expect_error(
    geos_write_wkt(geos_clip_by_rect(line, list(Inf, Inf, -Inf, -Inf))),
    "Clipping rectangle must be non-empty"
  )
})

test_that("geos_clip_by_rect() can use a wk::rct()", {
  line <- "LINESTRING (-1 5, 11 5)"
  expect_identical(
    geos_write_wkt(geos_clip_by_rect(c(NA, line), wk::rct(0, 0, 10, 10))),
    c(NA, "LINESTRING (0 5, 10 5)")
  )

  expect_error(
    geos_clip_by_rect(c(NA, line), wk::rct(0, 0, 10, 10, crs = 2928)),
    "are not equal"
  )
})

test_that("geos_buffer works", {
  expect_identical(geos_offset_curve(NA_character_, 1), geos_read_wkt(NA_character_))
  expect_identical(geos_offset_curve("LINESTRING (1 0, 3 0)", NA), geos_read_wkt(NA_character_))

  expect_equal(geos_xmin(geos_offset_curve("LINESTRING (1 0, 3 0)", 1)), 1)
  expect_equal(geos_ymin(geos_offset_curve("LINESTRING (1 0, 3 0)", 1)), 1)
  expect_equal(geos_xmax(geos_offset_curve("LINESTRING (1 0, 3 0)", 1)), 3)
  expect_equal(geos_ymax(geos_offset_curve("LINESTRING (1 0, 3 0)", 1)), 1)
})

test_that("geos_offset_curve works", {
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

test_that("triangulation works", {
  expect_true(
    geos_equals(
      geos_delaunay_triangles("MULTIPOINT (0 0, 1 0, 0 1)"),
      "POLYGON ((0 0, 1 0, 0 1, 0 0))"
    )
  )

  expect_true(
    geos_equals(
      geos_delaunay_edges("MULTIPOINT (0 0, 1 0, 0 1)"),
      "MULTILINESTRING ((0 1, 1 0), (0 0, 0 1), (0 0, 1 0))"
    )
  )

  expect_error(geos_delaunay_triangles("POINT (nan inf)"), "Unknown error|LocateFailureException")
  expect_error(geos_delaunay_edges("POINT (nan inf)"), "Unknown error|LocateFailureException")
  expect_identical(geos_delaunay_triangles(NA_character_), geos_read_wkt(NA_character_))
  expect_identical(geos_delaunay_edges(NA_character_), geos_read_wkt(NA_character_))
})

test_that("voronoi diagrams work", {
  expect_true(
    geos_equals(
      geos_voronoi_polygons("MULTIPOINT (0 0, 1 0, 0 3)"),
      geos_voronoi_polygons(
        "MULTIPOINT (0 0, 1 0, 0 3)",
        # this is the default env
        env = "POLYGON ((-1 -1, 2 -1, 2 4, -1 4, -1 -1))"
      )
    )
  )

  expect_true(
    geos_equals(
      geos_voronoi_edges("MULTIPOINT (0 0, 1 0, 0 3)"),
      geos_voronoi_edges(
        "MULTIPOINT (0 0, 1 0, 0 3)",
        # this is the default env
        env = "POLYGON ((-1 -1, 2 -1, 2 4, -1 4, -1 -1))"
      )
    )
  )

  expect_error(geos_voronoi_polygons("POINT (nan inf)"), "Unknown error|LocateFailureException")
  expect_error(geos_voronoi_edges("POINT (nan inf)"), "Unknown error|LocateFailureException")
  expect_identical(geos_voronoi_polygons(NA_character_), geos_read_wkt(NA_character_))
  expect_identical(geos_voronoi_edges(NA_character_), geos_read_wkt(NA_character_))

  bad_env <- geos_read_wkt("POLYGON EMPTY")
  tmp <- tempfile()
  saveRDS(bad_env, tmp)
  bad_env <- readRDS(tmp)
  unlink(tmp)
  expect_error(geos_voronoi_polygons("POINT EMPTY", env = bad_env), "not a valid external")
  expect_error(geos_voronoi_edges("POINT EMPTY", env = bad_env), "not a valid external")
})

test_that("child geometry works", {
  expect_identical(
    geos_write_wkt(
      geos_geometry_n(
        c(NA, "POINT (0 1)", "POINT (0 1)", "GEOMETRYCOLLECTION (POINT (0 1), POINT (1 2))"),
        c(1L, NA, 1L, 1L)
      )
    ),
    c(NA, NA, "POINT (0 1)", "POINT (0 1)")
  )

  # out-of-bounds checking
  expect_identical(
    geos_write_wkt(
      geos_geometry_n("GEOMETRYCOLLECTION (POINT (0 1), POINT (1 2))", 0:3)
    ),
    c(NA, "POINT (0 1)", "POINT (1 2)", NA)
  )
})

test_that("child rings works", {
  poly <- "POLYGON ((0 0, 0 1, 1 0, 0 0), (0.1 0.1, 0.1 0.2, 0.2 0.1, 0.1 0.1))"

  expect_identical(
    geos_write_wkt(
      geos_ring_n(c(NA, poly, poly), c(1, NA, 1))
    ),
    c(NA, NA, "LINEARRING (0 0, 0 1, 1 0, 0 0)")
  )

  expect_error(geos_ring_n("POINT (0 1)", 1), "Can't extract rings")

  # out-of-bounds checking
  expect_identical(
    geos_write_wkt(geos_ring_n(poly, 0:3)),
    c(NA, "LINEARRING (0 0, 0 1, 1 0, 0 0)", "LINEARRING (0.1 0.1, 0.1 0.2, 0.2 0.1, 0.1 0.1)", NA)
  )
})
