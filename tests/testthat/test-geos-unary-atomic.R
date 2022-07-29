
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
    geos_has_z(c("POINT Z (1 2 3)", "POINT (1 2)", NA)),
    c(TRUE, FALSE, NA)
  )

  expect_identical(
    geos_type_id(
      c(
        "POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
        "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
        "GEOMETRYCOLLECTION EMPTY", NA
      )
    ),
    c(1:7, NA)
  )

  expect_identical(
    geos_type(
      c(
        "POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
        "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
        "GEOMETRYCOLLECTION EMPTY", NA
      )
    ),
    c(
      "point", "linestring", "polygon",
      "multipoint", "multilinestring", "multipolygon",
      "geometrycollection", NA
    )
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
    geos_num_rings(
      c(
        "POLYGON ((0 0, 1 0, 0 1, 0 0))",
        "POLYGON ((0 0, 1 0, 0 1, 0 0), (0.1 0.1, 0.2 0.1, 0.1 0.2, 0.1 0.1))",
        NA
      )
    ),
    c(1L, 2L, NA)
  )

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

  expect_true(geos_is_clockwise("LINESTRING (0 0, 0 1, 1 0, 0 0)"))
  expect_false(geos_is_clockwise("LINESTRING (0 0, 1 0, 0 1, 0 0)"))
  expect_identical(
    geos_is_clockwise(c("POINT EMPTY", NA)),
    c(NA, NA)
  )

  expect_error(geos_is_clockwise("POINT (1 1)"), "IllegalArgumentException")
  expect_error(geos_is_clockwise("POLYGON ((0 0, 0 1, 1 0, 0 0))"), "must be a")
})

test_that("geos_hilbert_code() works", {
  skip_if_not(geos_version() >= "3.11.0")

  coords_grid <- expand.grid(x = 0:4, y = 0:4)
  geom_grid <- as_geos_geometry(wk::xy(coords_grid$x, coords_grid$y))

  expect_identical(
    geos_hilbert_code(c(geom_grid, geom_grid[NA_integer_]), level = 15),
    c(
      0L, 67108863L, 89478485L, 1006632959L, 1073741823L, 22369621L,
      44739242L, 111848106L, 939524096L, 1051372202L, 268435455L, 201326592L,
      178956970L, 872415232L, 805306368L, 335544319L, 313174698L, 469762048L,
      581610154L, 738197504L, 357913941L, 380283562L, 447392426L, 648719018L,
      715827882L,
      NA_integer_
    )
  )

  expect_identical(
    geos_hilbert_code("POINT (0 1)", NA_character_),
    NA_integer_
  )

  expect_identical(
    geos_hilbert_code("POINT (0 1)", level = NA_integer_),
    NA_integer_
  )

  expect_error(
    geos_hilbert_code("POINT (0 1)", level = 17),
    "IllegalArgumentException"
  )
})

test_that("validity checking works", {
  expect_identical(
    geos_is_valid(c("LINESTRING (0 0, 1 1)", "POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))", NA)),
    c(TRUE, FALSE, NA)
  )

  detail <- geos_is_valid_detail(
    c("LINESTRING (0 0, 1 1)", "POLYGON ((0 0, 1 1, 1 0, 0 1, 0 0))", NA)
  )

  expect_s3_class(detail, "data.frame")
  expect_named(detail, c("is_valid", "reason", "location"))
  expect_type(detail$is_valid, "logical")
  expect_type(detail$reason, "character")
  expect_s3_class(detail$location, "geos_geometry")
  expect_identical(geos_is_empty(detail$location), detail$is_valid)
})
