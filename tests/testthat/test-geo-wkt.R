
test_that("geo_wkt class works", {
  wkt <- geo_wkt("POINT (30 10)")
  expect_output(print(wkt), "geo_wkt")
  expect_output(print(tibble(wkt)), "wkt")
  expect_is(wkt, "geo_wkt")
  expect_true(is_geo_wkt(wkt))
  expect_true(vec_is(wkt))
  expect_equal(geo_size(wkt), 1)
})

test_that("parse problems for WKT are detected", {
  expect_identical(
    cpp_validate_provider(new_geo_wkt(c("POINT (30 10)", "POINT EMPTY", "MERR", "POINT FISH"))),
    c(TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(
    validate_geo_wkt(new_geo_wkt("POINT (30 10)")),
    new_geo_wkt("POINT (30 10)")
  )
  expect_error(
    validate_geo_wkt(new_geo_wkt("POINT FISH")),
    "1 geometry",
    class = "parse_error"
  )
  expect_error(
    validate_geo_wkt(rep(new_geo_wkt("POINT FISH"), 21)),
    "and 1 more",
    class = "parse_error"
  )
})

test_that("coersion and casting works for wkt types", {
  wkt <- geo_wkt("POINT (30 10)")

  expect_identical(vec_cast(wkt, geo_wkt()), wkt)
  expect_identical(vec_cast("POINT (30 10)", geo_wkt()), wkt)
  expect_identical(vec_cast(wkt, character()), "POINT (30 10)")
  expect_identical(as_geo_wkt("POINT (30 10)"), wkt)
  expect_error(as_geo_wkt("FISH"), class = "parse_error")
  expect_error(vec_cast("FISH", geo_wkt()), class = "parse_error")
  expect_error(as_geo_wkt(5), class = "vctrs_error_incompatible_cast")

  wkb <- vec_cast(wkt, geo_wkb())
  wkt_roundtrip <- vec_cast(wkb, geo_wkt())

  expect_match(wkt_roundtrip, "^POINT")
  expect_match(wkt_roundtrip[1], c("\\(30\\."))
  expect_match(wkt_roundtrip[1], c("10\\.0+\\)"))

  expect_identical(vec_cast(wkt, geo_wkb()), wkb)
  expect_identical(as_geo_wkt(wkb), wkt_roundtrip)
})
