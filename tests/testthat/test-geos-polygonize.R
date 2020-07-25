
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
