
test_that("wk_handle() works for points", {
  # ability to export an empty point in WKB changed in GEOS 3.9 so use WKT instead
  geoms <- as_geos_geometry(c("POINT (0 1)", "POINT Z (0 1 2)", "POINT EMPTY", NA))

  expect_identical(
    unclass(wk_handle(geoms, wk::wkt_writer())),
    unclass(geos_write_wkt(geoms))
  )

  geoms_srid <- geos_set_srid(geoms, 1234)
  expect_identical(wk::wk_meta(geoms_srid)$srid, c(1234L, 1234L, 1234L, NA))

  geoms_prec <- geos_set_precision(geoms, 0.1)
  expect_identical(wk::wk_meta(geoms_prec)$precision, c(0.1, 0.1, 0.1, NA))
})

test_that("wk_handle() works for linestrings", {
  # WKB export of LINESTRING EMPTY changed in GEOS 3.9 (before it was setting the Z flag)
  geoms <- as_geos_geometry(
    c("LINESTRING (0 1, 2 3)",
      "LINESTRING Z (0 1 2, 3 4 5)",
      "LINESTRING EMPTY", NA
    )
  )
  expect_identical(
    unclass(wk_handle(geoms, wk::wkt_writer())),
    unclass(geos_write_wkt(geoms))
  )

  geoms_srid <- geos_set_srid(geoms, 1234)
  expect_identical(wk::wk_meta(geoms_srid)$srid, c(1234L, 1234L, 1234L, NA))

  geoms_prec <- geos_set_precision(geoms, 0.1)
  expect_identical(wk::wk_meta(geoms_prec)$precision, c(0.1, 0.1, 0.1, NA))
})

test_that("wk_handle() works for polygons", {
  geoms <- as_geos_geometry(
    c("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))",
      "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 9 1, 9 9, 1 9, 1 1))",
      "POLYGON Z ((0 0 1, 10 0 1, 10 10 1, 0 10 1, 0 0 1))",
      "POLYGON Z ((0 0 1, 10 0 1, 10 10 1, 0 10 1, 0 0 1), (1 1 1, 9 1 1, 9 9 1, 1 9 1, 1 1 1))",
      "POLYGON EMPTY", NA
    )
  )
  expect_identical(
    unclass(wk_handle(geoms, wk::wkt_writer())),
    unclass(geos_write_wkt(geoms))
  )

  geoms_srid <- geos_set_srid(geoms, 1234)
  expect_identical(wk::wk_meta(geoms_srid)$srid, c(1234L, 1234L, 1234L, 1234L, 1234L, NA))

  geoms_prec <- geos_set_precision(geoms, 0.1)
  expect_identical(wk::wk_meta(geoms_prec)$precision, c(0.1, 0.1, 0.1, 0.1, 0.1, NA))
})

test_that("wk_handle() works for multipoints", {
  geoms <- as_geos_geometry(c("MULTIPOINT (0 1)", "MULTIPOINT Z (0 1 2)", "MULTIPOINT EMPTY", NA))
  expect_identical(
    unclass(wk_handle(geoms, wk::wkb_writer(endian = 1))),
    unclass(geos_write_wkb(geoms, endian = 1))
  )

  geoms_srid <- geos_set_srid(geoms, 1234)
  expect_identical(
    unclass(wk_handle(geoms_srid, wk::wkb_writer(endian = 1))),
    unclass(geos_write_wkb(geoms_srid, endian = 1, include_srid = TRUE))
  )

  geoms_prec <- geos_set_precision(geoms, 0.1)
  expect_identical(wk::wk_meta(geoms_prec)$precision, c(0.1, 0.1, 0.1, NA))
})

test_that("wk_handle() works for geometry collections", {
  geoms <- as_geos_geometry(
    c("GEOMETRYCOLLECTION Z (POINT Z (0 1 2), POINT Z (2 3 4))",
      "GEOMETRYCOLLECTION (MULTIPOINT (0 1))",
      "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (MULTIPOINT (0 1)))",
      "GEOMETRYCOLLECTION EMPTY", NA
    )
  )
  expect_identical(
    unclass(wk_handle(geoms, wk::wkb_writer(endian = 1))),
    unclass(geos_write_wkb(geoms, endian = 1))
  )

  geoms_srid <- geos_set_srid(geoms, 1234)
  expect_identical(
    unclass(wk_handle(geoms_srid, wk::wkb_writer(endian = 1))),
    unclass(geos_write_wkb(geoms_srid, endian = 1, include_srid = TRUE))
  )

  geoms_prec <- geos_set_precision(geoms, 0.1)
  expect_identical(wk::wk_meta(geoms_prec)$precision, c(0.1, 0.1, 0.1, 0.1, NA))
})

test_that("geos_geometry_writer() works for points", {
  expect_identical(
    geos_write_wkt(
      wk::wk_handle(
        wk::wkt(c("POINT EMPTY", "POINT (1 2)", "POINT Z (1 2 3)", NA)),
        geos_geometry_writer()
      )
    ),
    c("POINT EMPTY", "POINT (1 2)", "POINT Z (1 2 3)", NA)
  )
})

test_that("geos_geometry_writer() works for linestrings", {
  expect_identical(
    geos_write_wkt(
      wk::wk_handle(
        wk::wkt(c("LINESTRING EMPTY", "LINESTRING (1 2, 3 4)", "LINESTRING Z (1 2 3, 4 5 6)", NA)),
        geos_geometry_writer()
      )
    ),
    c("LINESTRING EMPTY", "LINESTRING (1 2, 3 4)", "LINESTRING Z (1 2 3, 4 5 6)", NA)
  )
})

test_that("geos_geometry can be created from wk package classes", {
  expect_s3_class(as_geos_geometry(wk::as_wkb("POINT (30 10)")), "geos_geometry")
  expect_s3_class(as_geos_geometry(wk::as_wkt("POINT (30 10)")), "geos_geometry")
  expect_s3_class(as_geos_geometry(wk::xy(30, 10)), "geos_geometry")
  expect_s3_class(as_geos_geometry(wk::xyz(30, 10, 11)), "geos_geometry")
  expect_s3_class(as_geos_geometry(wk::rct(1, 2, 3, 4)), "geos_geometry")
  expect_s3_class(as_geos_geometry(wk::crc()), "geos_geometry")
  # wk::crc() export must be fixed upstream ()
  # expect_s3_class(as_geos_geometry(wk::crc(30, 10, 10)), "geos_geometry")
})

test_that("coercion to wk::wkt, wk::wkb, wk::xy, and wk::xyz", {
  expect_identical(wk::as_wkt(as_geos_geometry("POINT (0 1)")), wk::wkt("POINT (0 1)"))
  expect_identical(wk::as_wkb(as_geos_geometry("POINT (0 1)")), wk::as_wkb("POINT (0 1)"))
  expect_identical(wk::as_xy(as_geos_geometry("POINT (0 1)")), wk::xy(0, 1))
  expect_equal(
    wk::as_xy(as_geos_geometry("POINT (0 1)"), dims = c("x", "y", "z")),
    wk::xyz(0, 1, NA_real_)
  )
  expect_equal(
    wk::as_xy(as_geos_geometry("POINT Z (0 1 2)"), dims = c("x", "y", "z")),
    wk::xyz(0, 1, 2)
  )
  expect_equal(
    wk::as_xy(as_geos_geometry("POINT Z (0 1 2)")),
    wk::xyz(0, 1, 2)
  )

  # check empty point conversion
  expect_identical(wk::as_wkt(as_geos_geometry("POINT EMPTY")), wk::as_wkt("POINT EMPTY"))
  expect_identical(wk::as_wkb(as_geos_geometry("POINT EMPTY")), wk::as_wkb("POINT EMPTY"))
  expect_equal(wk::as_xy(as_geos_geometry("POINT EMPTY")), wk::xy(NA_real_, NA_real_))
  expect_equal(
    wk::as_xy(as_geos_geometry("POINT EMPTY"), dims = c("x", "y", "z")),
    wk::xyz(NA_real_, NA_real_, NA_real_)
  )
})

test_that("crs can be fetched and set", {
  geom <- new_geos_geometry(list(NULL), crs = NULL)
  expect_null(wk_crs(geom))
  geom <- wk_set_crs(geom, 1234)
  expect_identical(wk_crs(geom), 1234)
})

test_that("crs propagates through conversion to wk::wkt(), wk::wkb(), and wk::xy()", {
  geom <- new_geos_geometry(list(NULL), crs = 1234)
  expect_identical(wk_crs(as_wkt(geom)), 1234)
  expect_identical(wk_crs(as_wkb(geom)), 1234)
  expect_identical(wk_crs(as_xy(geom)), 1234)
  expect_identical(wk_crs(as_xy(geom, dims = c("x", "y", "z"))), 1234)
})

test_that("crs propagates through conversion from wk::wkt(), wk::wkb(), wk::xy(), wk::rct(), and wk::crc()", {
  geom <- new_geos_geometry(list(NULL), crs = 1234)
  expect_identical(as_geos_geometry(wk::wkt(NA_character_, crs = 1234)), geom)
  expect_identical(as_geos_geometry(wk::wkb(list(NULL), crs = 1234)), geom)
  expect_identical(as_geos_geometry(wk::xy(crs = 1234)), geom[integer(0)])
  expect_identical(as_geos_geometry(wk::rct(crs = 1234)), geom[integer(0)])
  expect_identical(as_geos_geometry(wk::crc(crs = 1234)), geom[integer(0)])
})
