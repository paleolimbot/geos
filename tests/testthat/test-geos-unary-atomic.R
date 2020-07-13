
test_that("atomic extractors work", {
  expect_identical(
    geos_area(c("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", NA)),
    c(100, NA)
  )

  expect_identical(
    geos_length(c("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", NA)),
    c(40, NA)
  )

  expect_identical(geos_x(c("POINT Z (1 2 3)", NA)), c(1, NA))
  expect_identical(geos_y(c("POINT Z (1 2 3)", NA)), c(2, NA))
  expect_identical(geos_z(c("POINT Z (1 2 3)", NA)), c(3, NA))
  expect_error(geos_x("LINESTRING (0 1, 1 2)"), "Argument is not a Point")

  expect_identical(geos_xmin(c("LINESTRING (0 1, 2 3)", NA)), c(0, NA))
  expect_identical(geos_ymin(c("LINESTRING (0 1, 2 3)", NA)), c(1, NA))
  expect_identical(geos_xmax(c("LINESTRING (0 1, 2 3)", NA)), c(2, NA))
  expect_identical(geos_ymax(c("LINESTRING (0 1, 2 3)", NA)), c(3, NA))

  expect_identical(
    geos_minimum_clearance(c("POLYGON ((0 0, 10 0, 10 10, 3 5, 0 10, 0 0))", NA)),
    c(3, NA)
  )
})

test_that("atomic returners work", {
  expect_identical(geos_is_empty(c("POINT EMPTY", "POINT (0 1)", NA)), c(TRUE, FALSE, NA))
  expect_identical(
    geos_is_simple(c("LINESTRING (0 0, 1 1)", "LINESTRING (0 0, 1 1, 1 0, 0 1)", NA)),
    c(TRUE, FALSE, NA)
  )
  expect_identical(
    geos_is_ring(c("LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)", "POINT (0 1)", NA)),
    c(TRUE, FALSE, NA)
  )
  expect_identical(
    geos_is_closed(c("LINESTRING (0 0, 1 0, 1 1, 0 1, 0 0)", "LINESTRING (0 0, 1 1)", NA)),
    c(TRUE, FALSE, NA)
  )
  expect_error(geos_is_closed("POINT (0 1)"), "Argument is not a LineString")

  expect_identical(
    geos_is_valid(c("LINESTRING (0 0, 1 1)", "POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))", NA)),
    c(TRUE, FALSE, NA)
  )

  expect_identical(
    geos_has_z(c("POINT Z (1 2 3)", "POINT (1 2)", NA)),
    c(TRUE, FALSE, NA)
  )

  expect_identical(
    geos_type_id(c("POINT (0 0)", "LINESTRING (0 0, 1 1)", NA)),
    c(1L, 2L, NA)
  )

  # haven't implemented set_precision yet
  expect_identical(geos_precision(c("POINT (0 0)", NA)), c(0, NA))

  expect_identical(
    geos_srid(wk::as_wkb(c("SRID=1234;POINT (0 0)", "POINT (0 0)", NA))),
    c(1234L, 0L, NA)
  )

  expect_identical(
    geos_num_coordinates(c("POINT (0 0)", "MULTIPOINT (0 0, 1 1)", NA)),
    c(1L, 2L, NA)
  )

  expect_identical(
    geos_num_geometries(c("POINT (0 0)", "MULTIPOINT (0 0, 1 1)", NA)),
    c(1L, 2L, NA)
  )

  expect_identical(
    geos_num_interior_rings(
      c(
        "POLYGON ((0 0, 1 0, 0 1, 0 0))",
        "POLYGON ((0 0, 1 0, 0 1, 0 0), (0.1 0.1, 0.2 0.1, 0.1 0.2, 0.1 0.1))",
        NA
      )
    ),
    c(0L, 1L, NA)
  )
  expect_error(geos_num_interior_rings("POINT (0 1)"), "not a Polygon")

  expect_identical(
    geos_dimension(c("POINT (0 0)", "LINESTRING (0 0, 1 1)", NA)),
    c(0L, 1L, NA)
  )
  # not sure what this is about
  expect_error(geos_dimension("GEOMETRYCOLLECTION EMPTY"), "Unknown error")

  expect_identical(
    geos_coordinate_dimension(c("POINT (0 0)", "POINT Z (0 0 1)", NA)),
    c(2L, 3L, NA)
  )
})

