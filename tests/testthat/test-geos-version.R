
test_that("geos_version works", {
  expect_length(geos_version(), 1)
  expect_match(geos_version(), "3\\.[0-9]{1,2}\\.[0-9]{1,2}")
  expect_match(geos_capi_version(), "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}")
})

test_that("GEOS errors are properly thrown", {
  skip("Don't know how to get .stop_geos() called")
  expect_error(geos_test_throw_error(), class = "geos_error")
})
