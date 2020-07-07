
test_that("WKT reader works", {
  # regular read/write
  expect_is(geos_read_wkt("POINT (30 10)"), "geos_geometry")
  expect_identical(geos_write_wkt(geos_read_wkt("POINT Z (30 10 2)")), "POINT Z (30 10 2)")

  # NULL/NA read/write
  expect_identical(geos_write_wkt(new_geos_geometry(list(NULL))), NA_character_)
  expect_identical(geos_read_wkt(NA_character_), new_geos_geometry(list(NULL)))

  # read/write when the internal pointer is NULL
  temp_rds <- tempfile()
  saveRDS(geos_read_wkt("POINT EMPTY"), temp_rds)
  expect_error(geos_write_wkt(readRDS(temp_rds)), "External pointer is not valid")
  unlink(temp_rds)

  # error parse
  expect_error(geos_read_wkt("NOPE"), "ParseException")
})
