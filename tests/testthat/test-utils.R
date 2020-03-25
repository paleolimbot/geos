
test_that("rep_along_or_fail function works", {
  expect_identical(rep_along_or_fail(1, 1:3), c(1, 1, 1))
  expect_identical(rep_along_or_fail(1:3, 1:3), c(1L, 2L, 3L))
  expect_error(rep_along_or_fail(1:2, 1:3), class = "rep_len_error")
})

test_that("rep_len_or_fail function works", {
  expect_identical(rep_len_or_fail(1, 3), c(1, 1, 1))
  expect_identical(rep_len_or_fail(1:3, 3), c(1L, 2L, 3L))
  expect_identical(rep_len_or_fail(1:3, 1), c(1L, 2L, 3L))
  expect_error(rep_len_or_fail(1:2, 3), class = "rep_len_error")
})

test_that("geos asserter works", {
  expect_error(assert_geos_version("10.0.0"), "is required")
})
