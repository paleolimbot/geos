
test_that("geo_wkt class works", {
  wkt <- geo_wkt("POINT (30 10)")
  expect_output(print(wkt), "geo_wkt")
  expect_output(print(tibble(wkt)), "wkt")
  expect_is(wkt, "geo_wkt")
  expect_true(is_geo_wkt(wkt))
  expect_true(vec_is(wkt))
})
