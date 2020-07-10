
test_that("WKT reader works", {
  # regular read/write
  expect_is(geos_read_wkt("POINT (30 10)"), "geos_geometry")
  expect_identical(geos_write_wkt(geos_read_wkt("POINT Z (30 10 2)")), "POINT Z (30 10 2)")

  # options
  expect_identical(
    geos_write_wkt(geos_read_wkt("POINT Z (30 10 2)"), include_z = TRUE),
    "POINT Z (30 10 2)"
  )
  expect_identical(
    geos_write_wkt(geos_read_wkt("POINT Z (30 10 2)"), include_z = FALSE),
    "POINT (30 10)"
  )
  expect_identical(
    geos_write_wkt(geos_read_wkt("POINT Z (30 10 2)"), precision = 2, trim = FALSE),
    "POINT Z (30.00 10.00 2.00)"
  )

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

test_that("WKB reader works", {
  # regular read/write
  expect_is(geos_read_wkb(wk::wkt_translate_wkb("POINT (30 10)")), "geos_geometry")
  expect_identical(
    geos_write_wkb(geos_read_wkt("POINT Z (30 10 2)")),
    structure(wk::wkt_translate_wkb("POINT Z (30 10 2)"), class = "blob")
  )

  # options
  expect_identical(
    geos_write_wkb(geos_read_wkt("POINT (0 0)"), endian = 1)[[1]][1],
    as.raw(0x01)
  )
  expect_identical(
    geos_write_wkb(geos_read_wkt("POINT (0 0)"), endian = 0)[[1]][1],
    as.raw(0x00)
  )
  expect_identical(
    geos_write_wkb(geos_read_wkt("POINT Z (30 10 2)"), include_z = TRUE),
    structure(wk::wkt_translate_wkb("POINT Z (30 10 2)"), class = "blob")
  )
  expect_identical(
    geos_write_wkb(geos_read_wkt("POINT Z (30 10 2)"), include_z = FALSE),
    structure(wk::wkt_translate_wkb("POINT (30 10)"), class = "blob")
  )
  expect_identical(
    geos_write_wkb(geos_read_wkb(wk::wkt_translate_wkb("SRID=123;POINT (30 10)")), include_srid = TRUE),
    structure(wk::wkt_translate_wkb("SRID=123;POINT (30 10)"), class = "blob")
  )
  expect_identical(
    geos_write_wkb(geos_read_wkb(wk::wkt_translate_wkb("SRID=123;POINT (30 10)")), include_srid = FALSE),
    structure(wk::wkt_translate_wkb("POINT (30 10)"), class = "blob")
  )

  # NULL/NA read/write
  expect_identical(
    geos_write_wkb(new_geos_geometry(list(NULL))),
    structure(list(NULL), class = "blob")
  )
  expect_identical(geos_read_wkb(list(NULL)), new_geos_geometry(list(NULL)))

  # read/write when the internal pointer is NULL
  temp_rds <- tempfile()
  saveRDS(geos_read_wkt("POINT (0 0)"), temp_rds)
  expect_error(geos_write_wkb(readRDS(temp_rds)), "External pointer is not valid")
  unlink(temp_rds)

  # attempt to write empty point
  expect_error(
    geos_write_wkb(geos_read_wkt("POINT EMPTY")),
    "Empty Points cannot be represented"
  )
})
