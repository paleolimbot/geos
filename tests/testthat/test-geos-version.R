
test_that("geos_version works", {
  expect_length(geos_version(), 1)
  expect_match(as.character(geos_version()), "3\\.[0-9]{1,2}\\.[0-9]{1,2}")
  expect_match(as.character(geos_capi_version()), "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}")
})
