
test_that("geos_version works", {
  expect_true(geos_version() >= "3.8.1")
  expect_true(geos_version(runtime = FALSE) >= "3.8.1")
})
