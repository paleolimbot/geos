
test_that("geos_xptr class works", {
  expect_s3_class(new_geos_xptr(list()), "geos_xptr")
  expect_s3_class(new_geos_xptr(list(NULL)), "geos_xptr")
  expect_error(new_geos_xptr(NULL), "must be a bare list")
})

test_that("geos_xptr validation works", {
  expect_identical(validate_geos_xptr(new_geos_xptr(list())), new_geos_xptr(list()))
  expect_identical(validate_geos_xptr(new_geos_xptr(list(NULL))), new_geos_xptr(list(NULL)))
  expect_error(validate_geos_xptr(list("wrong type")), "must be externalptr")
})

test_that("geos_xptr subsetting and concatenation work", {
  xptr <- new_geos_xptr(list(NULL, NULL))
  expect_identical(xptr[1], new_geos_xptr(list(NULL)))
  expect_identical(xptr[[1]], xptr[1])
  expect_identical(c(xptr, xptr), new_geos_xptr(list(NULL, NULL, NULL, NULL)))
  expect_identical(rep(xptr, 2), new_geos_xptr(list(NULL, NULL, NULL, NULL)))
  expect_identical(rep_len(xptr, 4), new_geos_xptr(list(NULL, NULL, NULL, NULL)))
})

test_that("geos_xptr default print method works", {
  expect_output(print(new_geos_xptr()), "geos_xptr")
  expect_output(print(new_geos_xptr(list(NULL))), "geos_xptr")
})
