
test_that("geo_wkt class works", {
  wkt <- geo_wkt("POINT (30 10)")
  expect_output(print(wkt), "geo_wkt")
  expect_output(print(tibble(wkt)), "wkt")
  expect_is(wkt, "geo_wkt")
  expect_true(is_geo_wkt(wkt))
  expect_true(vec_is(wkt))
})

test_that("parse problems for WKT are detected", {
  expect_identical(
    geos_wkt_is_parseable(c("POINT (30 10)", "POINT EMPTY", "MERR", "POINT FISH")),
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


