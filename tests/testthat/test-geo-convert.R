
test_that("wkt conversion works", {
  wkt <- geo_wkt(c("POINT (30 10)", "POINT (20 20)"))
  wkt_roundtrip <- geos_test_roundtrip_wkt(wkt)
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
  wkb_roundtrip <- new_geo_wkb(vec_cast(geos_test_roundtrip_wkb(wkb), list_of(.ptype = raw())))
  expect_identical(wkb, wkb_roundtrip)
})
