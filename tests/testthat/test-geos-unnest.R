
test_that("geos_unnest() works", {
  expect_identical(
    geos_unnest(NA_character_),
    structure(list(NULL), class = "geos_geometry", lengths = 1L)
  )

  unnested <- geos_unnest(
    "GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)",
    keep_multi = FALSE, keep_empty = FALSE, max_depth = 2
  )
  expect_identical(
    geos_write_wkt(unnested),
    c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)")
  )
  expect_identical(attr(unnested, "lengths"), 3L)

  unnested <- geos_unnest(
    "GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)",
    keep_multi = FALSE, keep_empty = TRUE, max_depth = 2
  )
  expect_identical(
    geos_write_wkt(unnested),
    c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)", "GEOMETRYCOLLECTION EMPTY")
  )
  expect_identical(attr(unnested, "lengths"), 4L)
})

test_that("wk*_unnest(max_depth) is respected", {
  unnested <-  geos_unnest(
    "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
    max_depth = 0
  )
  expect_identical(
    geos_write_wkt(unnested),
    "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))"
  )
  expect_identical(attr(unnested, "lengths"), 1L)

  unnested <-  geos_unnest(
    "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
    max_depth = 1
  )
  expect_identical(
    geos_write_wkt(unnested),
    "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1)))"
  )
  expect_identical(attr(unnested, "lengths"), 1L)

  unnested <-  geos_unnest(
    "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
    max_depth = 2
  )
  expect_identical(
    geos_write_wkt(unnested),
    "GEOMETRYCOLLECTION (POINT (0 1))"
  )
  expect_identical(attr(unnested, "lengths"), 1L)

  unnested <-  geos_unnest(
    "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
    max_depth = 3
  )
  expect_identical(
    geos_write_wkt(unnested),
    "POINT (0 1)"
  )
  expect_identical(attr(unnested, "lengths"), 1L)

  unnested <-  geos_unnest(
    "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
    max_depth = 4
  )
  expect_identical(
    geos_write_wkt(unnested),
    "POINT (0 1)"
  )
  expect_identical(attr(unnested, "lengths"), 1L)
})
