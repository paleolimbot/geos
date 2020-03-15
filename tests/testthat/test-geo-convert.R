
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
    new_geo_tbl_point()
  )
  tbl <- geo_restore(raw)

  expect_identical(validate_geo_tbl_point(tbl), tbl)
  expect_equal(vec_size(tbl), 1)
  expect_true(all(field(tbl, "feature") == 1L))
})

test_that("geo_tbl_linestring conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("LINESTRING (30 10, 10 30, 40 40)"),
    new_geo_tbl_linestring()
  )
  tbl <- geo_restore(raw)

  expect_identical(validate_geo_tbl_linestring(tbl), tbl)
  expect_equal(vec_size(tbl), 3)
  expect_true(all(field(tbl, "feature") == 1L))
})

test_that("geo_tbl_multipoint conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("MULTIPOINT ((10 40), (40 30))"),
    new_geo_tbl_multipoint()
  )
  tbl <- geo_restore(raw)

  expect_identical(validate_geo_tbl_multipoint(tbl), tbl)
  expect_equal(vec_size(tbl), 2)
  expect_true(all(field(tbl, "feature") == 1L))
  expect_setequal(field(tbl, "part"), c(1L, 2L))
  tbl <- geo_restore(raw)
})

test_that("geo_tbl_multilinestring conversion works", {
  raw <- geomcpp_convert(
    geo_wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))"),
    new_geo_tbl_linestring()
  )
  tbl <- geo_restore(raw)

  expect_identical(validate_geo_tbl_multilinestring(tbl), tbl)
  expect_equal(vec_size(tbl), 7)
  expect_setequal(field(tbl, "part"), c(1L, 2L))
})

test_that("error occurs with unknown object in conversions", {
  expect_error(geomcpp_convert(NULL, new_geo_wkt()), "Can't resolve")
})
