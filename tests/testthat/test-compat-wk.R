
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
