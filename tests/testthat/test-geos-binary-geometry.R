
test_that("pattern for erroring on mismatched CRS works", {
  expect_error(
    geos_intersection(
      as_geos_geometry("POINT (1 1)", crs = 1234),
      as_geos_geometry("POINT (1 1)", crs = 5678)
    ),
    "are not equal"
  )
})

test_that("pattern for propagating CRS works", {
  expect_identical(
    wk::wk_crs(
      geos_intersection(
        as_geos_geometry("POINT (1 1)", crs = 1234),
        as_geos_geometry("POINT (1 1)", crs = 1234)
      )
    ),
    1234
  )
})

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

test_that("binary_prec operators work", {
  poly1 <- "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
  poly2 <- c(NA, "POLYGON ((5 5, 5 15, 15 15, 15 5, 5 5))")

  if ((geos_version(runtime = TRUE) >= "3.9.1") && (geos_version(runtime = FALSE) >= "3.9.1")) {
    expect_identical(
      geos_area(geos_intersection_prec(poly1, poly2, grid_size = 0.1)),
      c(NA, 25)
    )

    expect_identical(
      geos_area(geos_difference_prec(poly1, poly2, grid_size = 0.1)),
      c(NA, 100 - 25)
    )

    expect_identical(
      geos_area(geos_sym_difference_prec(poly1, poly2, grid_size = 0.1)),
      c(NA, 100 * 2 - 50)
    )

    expect_identical(
      geos_area(geos_union_prec(poly1, poly2, grid_size = 0.1)),
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
        geos_unary_union_prec(c(NA, collection), grid_size = 0.1),
        geos_union_prec(poly1, poly2, grid_size = 0.1)
      ),
      c(NA, TRUE)
    )
  } else if (geos_version(runtime = FALSE) >= "3.9.1") {
    expect_error(geos_intersection_prec(poly1, poly2, 1), "requires 'libgeos'")
    expect_error(geos_difference_prec(poly1, poly2, 1), "requires 'libgeos'")
    expect_error(geos_sym_difference_prec(poly1, poly2, 1), "requires 'libgeos'")
    expect_error(geos_union_prec(poly1, poly2, 1), "requires 'libgeos'")
  } else {
    expect_error(geos_intersection_prec(poly1, poly2, 1), "built against 'libgeos'")
    expect_error(geos_difference_prec(poly1, poly2, 1), "built against 'libgeos'")
    expect_error(geos_sym_difference_prec(poly1, poly2, 1), "built against 'libgeos'")
    expect_error(geos_union_prec(poly1, poly2, 1), "built against 'libgeos'")
  }
})

test_that("shared paths works", {
  expect_identical(
    geos_write_wkt(
      geos_shared_paths("LINESTRING (0 0, 1 1, 2 2)", c(NA, "LINESTRING (1 1, 2 2, 3 3)"))
    ),
    c(NA, "GEOMETRYCOLLECTION (MULTILINESTRING ((1 1, 2 2)), MULTILINESTRING EMPTY)")
  )

  expect_identical(
    geos_write_wkt(
      geos_shared_paths("LINESTRING (0 0, 1 1, 2 2)", c(NA, "LINESTRING (3 3, 2 2, 1 1)"))
    ),
    c(NA, "GEOMETRYCOLLECTION (MULTILINESTRING EMPTY, MULTILINESTRING ((1 1, 2 2)))")
  )

  expect_error(geos_shared_paths("POINT (0 0)", "LINESTRING EMPTY"), "Geometry is not lineal")
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


test_that("clearance line between works", {
  expect_identical(
    geos_write_wkt(
      geos_clearance_line_between(
        "POINT (5 5)",
        c(NA, "POINT (1 1)", "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
      )
    ),
    c(NA, "LINESTRING (5 5, 1 1)", "LINESTRING (5 5, 5 5)")
  )

  expect_true(
    geos_is_empty(
      geos_clearance_line_between("POINT (0 0)", "POINT EMPTY")
    )
  )

  expect_error(
    geos_clearance_line_between("POINT (nan inf)", "POINT (0 0)"),
    "Unknown error"
  )
})
