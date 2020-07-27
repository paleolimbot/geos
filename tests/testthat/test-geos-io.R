
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

  # this specifically is firing an error longer than the buffer used to store the error (1024 chars)
  # to make sure this doesn't crash R
  really_long_bad_wkt <- strrep("A", 2048)
  expect_error(geos_read_wkt(really_long_bad_wkt), "ParseException")
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

  # attempt to read invalid WKB
  wkb <- wk::wkt_translate_wkb("POINT (1 1)", endian = 1)
  wkb[[1]][3] <- as.raw(0xff)
  expect_error(geos_read_wkb(wkb), "Unknown WKB type")
})

test_that("hex reader/writer works", {
  expect_identical(
    geos_write_wkt(
      geos_read_hex(c(NA, "01010000000000000000000000000000000000f03F"))
    ),
    c(NA, "POINT (0 1)")
  )

  expect_identical(
    geos_write_hex(c(NA, "POINT (0 1)"), endian = 1),
    c(NA, "01010000000000000000000000000000000000F03F")
  )

  # options
  expect_match(geos_write_hex("POINT (0 1)", endian = 0), "^00")

  expect_identical(
    geos_write_hex(geos_set_srid("POINT (0 1)", 1), include_srid = FALSE),
    geos_write_hex("POINT (0 1)")
  )

  expect_identical(
    geos_srid(
      geos_read_hex(
        geos_write_hex(geos_set_srid("POINT (0 1)", 1), include_srid = TRUE)
      )
    ),
    1L
  )

  expect_identical(
    geos_write_hex("POINT Z (0 1 2)", include_z = FALSE),
    geos_write_hex("POINT (0 1)")
  )

  expect_identical(
    geos_write_wkt(geos_read_hex(geos_write_hex("POINT Z (0 1 2)", include_z = TRUE))),
    "POINT Z (0 1 2)"
  )

  expect_error(geos_read_hex("not hex"), "ParseException")
  expect_error(geos_write_hex("POINT EMPTY"), "IllegalArgumentException")
})

test_that("xy reader/writer works", {
  expect_identical(
    geos_write_wkt(geos_read_xy(list(c(0, 0, 0, NA), c(1:3, NA)))),
    c("POINT (0 1)", "POINT (0 2)", "POINT (0 3)", "POINT EMPTY")
  )

  expect_identical(
    geos_write_xy(geos_read_wkt(c("POINT (0 1)", "POINT (0 2)", "POINT (0 3)", "POINT EMPTY"))),
    list(x = c(0, 0, 0, NA), y = as.numeric(c(1:3, NA)))
  )

  expect_identical(geos_write_xy(new_geos_geometry(list(NULL))), list(x = NA_real_, y = NA_real_))
  expect_error(geos_write_xy(geos_read_wkt("LINESTRING (0 0, 1 1)")), "Argument is not a Point")
})

test_that("empty creator works", {
  expect_error(geos_empty(8), "Unsupported type request")

  expect_identical(
    geos_write_wkt(
      geos_empty(
        c(
          "point", "linestring", "polygon",
          "multipoint", "multilinestring", "multipolygon",
          "geometrycollection", NA
        )
      )
    ),
    c(
      "POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY", NA
    )
  )

  expect_identical(
    geos_write_wkt(
      geos_empty(c(1:7, NA))
    ),
    c(
      "POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY", NA
    )
  )

  expect_identical(
    geos_write_wkt(
      geos_empty(
        geos_read_wkt(
          c(
            "POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
            "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
            "GEOMETRYCOLLECTION EMPTY", NA
          )
        )
      )
    ),
    c(
      "POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY", NA
    )
  )
})
