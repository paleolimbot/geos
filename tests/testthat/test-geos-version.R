
test_that("multiplication works", {
  expect_length(geos_version(), 1)
  expect_match(geos_version(), "3\\.[0-9]{1,2}\\.[0-9]{1,2}")
  expect_match(geos_capi_version(), "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}")
})
