
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
  expect_equal(geo_size(wkb), 1)
})

test_that("geo_wkb parse validation works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  # scrambled a bit
  wkb_bad <- as.raw(
    c(
      0xFF, 0xE9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x044, 0x3e, 0x40, 0x28, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40, 0xFF, 0xF2
    )
  )

  wkb <- new_geo_wkb(vec_cast(list(wkb_raw, wkb_bad), list_of(.ptype = raw())))
  expect_identical(cpp_validate_provider(wkb), c(TRUE, FALSE))

  expect_identical(validate_geo_wkb(wkb[1]), wkb[1])
  expect_error(validate_geo_wkb(wkb), "1 geometry", class = "parse_error")
})

test_that("wkb casting and coersion works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  # scrambled a bit
  wkb_bad <- as.raw(
    c(
      0xFF, 0xE9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x044, 0x3e, 0x40, 0x28, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40, 0xFF, 0xF2
    )
  )

  wkb <- geo_wkb(list(wkb_raw))

  expect_identical(vec_cast(wkb, geo_wkb()), wkb)
  expect_identical(vec_cast(list(wkb_raw), geo_wkb()), wkb)
  expect_identical(vec_cast(list(wkb_raw), geo_wkb()), as_geo_wkb(wkb))
  expect_identical(vec_cast(wkb, list()), list(wkb_raw))
  expect_identical(as_geo_wkb(list(wkb_raw)), wkb)
  expect_error(as_geo_wkb(list(wkb_bad)), class = "parse_error")
  expect_error(vec_cast(list(wkb_bad), geo_wkb()), class = "parse_error")
  expect_error(as_geo_wkb(5), class = "vctrs_error_incompatible_cast")

  wkt <- vec_cast(wkb, geo_wkt())
  wkb_roundtrip <- vec_cast(wkt, geo_wkb())
  expect_identical(wkb, wkb_roundtrip)
  expect_identical(as_geo_wkt(wkb), wkt)
  expect_identical(as_geo_wkb(wkt), wkb)
})
