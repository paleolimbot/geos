
test_that("geo_wkb class works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )
  wkb <- geo_wkb(list(wkb_raw))
  expect_output(print(wkb), "geo_wkb")
  expect_output(print(tibble(wkb)), "wkb")
  expect_is(wkb, "geo_wkb")
  expect_true(is_geo_wkb(wkb))
  expect_true(vec_is(wkb))
})
