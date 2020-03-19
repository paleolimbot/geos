
test_that("geo_is_empty() works", {
  expect_true(geo_is_empty(geo_wkt("POINT EMPTY")))
  expect_false(geo_is_empty(geo_wkt("POINT (30 10)")))
})
