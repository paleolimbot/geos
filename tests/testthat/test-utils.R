
test_that("rep_len or fail function works", {
  expect_identical(rep_len_or_fail(1, 1:3), c(1, 1, 1))
  expect_identical(rep_len_or_fail(1:3, 1:3), c(1L, 2L, 3L))
  expect_error(rep_len_or_fail(1:2, 1:3), class = "rep_len_error")
})
