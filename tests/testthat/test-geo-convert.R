
test_that("wkt conversion works", {
  wkt <- geo_wkt(c("POINT (30 10)", "POINT (20 20)"))
  wkt_roundtrip <- geomcpp_convert(wkt, new_geo_wkt())
  expect_match(wkt_roundtrip, "^POINT")
  expect_match(wkt_roundtrip[1], c("\\(30\\."))
  expect_match(wkt_roundtrip[1], c("10\\.0+\\)"))
  expect_match(wkt_roundtrip[2], c("\\(20\\."))
})

test_that("wkb conversion works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  wkb <- geo_wkb(list(wkb_raw))
  wkb_roundtrip <- new_geo_wkb(vec_cast(geomcpp_convert(wkb, new_geo_wkb()), list_of(.ptype = raw())))
  expect_identical(wkb, wkb_roundtrip)
})

test_that("geo_tbl_point conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("POINT (10 40)"),
    geo_tbl()
  )
  tbl <- geo_restore(raw)
  expect_identical(tbl, geo_tbl_point(geo_xy(10, 40)))
})

test_that("geo_tbl_linestring conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("LINESTRING (30 10, 10 30, 40 40)"),
    geo_tbl()
  )
  tbl <- geo_restore(raw)

  expect_identical(
    tbl,
    geo_tbl_linestring(geo_xy(c(30, 10, 40), c(10, 30, 40)))
  )
})

test_that("geo_tbl_multipoint conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("MULTIPOINT ((10 40), (40 30))"),
    geo_tbl()
  )
  tbl <- geo_restore(raw)

  expect_identical(
    tbl,
    geo_tbl_multipoint(geo_xy(c(10, 40), c(40, 30)), feature = 1)
  )
})

test_that("geo_tbl_multilinestring conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))"),
    geo_tbl()
  )
  tbl <- geo_restore(raw)

  expect_identical(
    tbl,
    geo_tbl_multilinestring(
      geo_xy(
        c(10, 20, 10, 40, 30, 40, 30),
        c(10, 20, 40, 40, 30, 20, 10)
      ),
      feature = 1,
      part = c(1, 1, 1, 2, 2, 2, 2)
    )
  )
})

test_that("geo_tbl_polygon conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))"),
    geo_tbl()
  )
  tbl <- geo_restore(raw)

  expect_identical(
    tbl,
    geo_tbl_polygon(
      geo_xy(c(30, 10, 40, 30), c(10, 30, 40, 10))
    )
  )
})

test_that("geo_tbl_multi_polygon conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)), ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))"),
    geo_tbl()
  )
  tbl <- geo_restore(raw)

  expect_identical(
    tbl,
    geo_tbl_multipolygon(
      geo_xy(
        c(40, 20, 45, 40, 20, 10, 10, 30, 45, 20, 30, 20, 20, 30),
        c(40, 45, 30, 40, 35, 30, 10, 5,  20, 35, 20, 15, 25, 20)
      ),
      part = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
      piece = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2)
    )
  )
})

test_that("error occurs with unknown object in conversions", {
  expect_error(geomcpp_convert(NULL, new_geo_wkt()), "Can't resolve")
})
