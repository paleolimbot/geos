
test_that("polygonize works", {
  expect_true(
    geos_equals(
      geos_polygonize("MULTILINESTRING ((0 0, 0 1), (0 1, 1 0), (1 0, 0 0))"),
      "POLYGON ((0 0, 0 1, 1 0, 0 0))"
    )
  )

  expect_identical(geos_polygonize(NA_character_), geos_read_wkt(NA_character_))

  bad_ptr <- geos_read_wkt("POINT (0 0)")
  tmp <- tempfile()
  saveRDS(bad_ptr, tmp)
  bad_ptr <- readRDS(tmp)
  unlink(tmp)
  expect_error(geos_polygonize(bad_ptr), "not a valid external pointer")
})

test_that("polygonize valid works", {
  # don't have a good example of how these are different
  expect_true(
    geos_equals(
      geos_polygonize("MULTILINESTRING ((0 0, 0 1), (0 1, 1 0), (1 0, 0 0))"),
      geos_polygonize_valid("MULTILINESTRING ((0 0, 0 1), (0 1, 1 0), (1 0, 0 0))")
    )
  )
})

test_that("polygonize cut edges works", {
  # don't have a good example of how to create a cut edge here
  expect_true(
    geos_equals(
      geos_polygonize_cut_edges("MULTILINESTRING ((0 0, 0 1), (0 1, 1 0), (1 0, 0 0))"),
      "GEOMETRYCOLLECTION EMPTY"
    )
  )
})

test_that("polygonize full works", {
  poly_valid <- geos_polygonize_full("MULTILINESTRING ((0 0, 0 1), (0 1, 1 0), (1 0, 0 0))")
  expect_true(
    geos_equals(
      poly_valid$result,
      geos_polygonize("MULTILINESTRING ((0 0, 0 1), (0 1, 1 0), (1 0, 0 0))")
    )
  )

  expect_true(geos_equals(poly_valid$cut_edges, geos_empty()))
  expect_true(geos_equals(poly_valid$dangles, geos_empty()))
  expect_true(geos_equals(poly_valid$invalid_rings, geos_empty()))
})
