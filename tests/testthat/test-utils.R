
test_that("sanitizers work", {
  expect_identical(sanitize_geos_geometry(geos_geometry()), geos_geometry())
  expect_identical(sanitize_geos_geometry(character()), geos_geometry(crs = NULL))
  expect_s3_class(sanitize_geos_strtree(geos_strtree(geos_geometry())), "geos_strtree")
  expect_s3_class(sanitize_geos_strtree(geos_geometry()), "geos_strtree")
  expect_identical(sanitize_double(1), 1)
  expect_identical(sanitize_double(1L), 1)
  expect_identical(sanitize_integer(1L), 1L)
  expect_identical(sanitize_integer(1), 1L)
  expect_identical(sanitize_double_scalar(1), 1)
  expect_identical(sanitize_double_scalar(1L), 1)
  expect_identical(sanitize_integer_scalar(1L), 1L)
  expect_identical(sanitize_integer_scalar(1), 1L)
})

test_that("recycle_common works", {
  expect_identical(recycle_common(list(1, 2)), list(1, 2))
  expect_identical(recycle_common(list(1, b = 2)), list(1, b = 2))
  expect_identical(recycle_common(list(1, 2:4)), list(c(1, 1, 1), c(2L, 3L, 4L)))
  expect_identical(recycle_common(list(numeric(0), 2)), list(numeric(0), numeric(0)))
  expect_error(recycle_common(list(numeric(0), 2:4)), "Incompatible lengths")
})
