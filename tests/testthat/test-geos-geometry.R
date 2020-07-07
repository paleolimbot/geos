
test_that("geos_geometry class works", {
  expect_s3_class(new_geos_geometry(list()), "geos_geometry")
  expect_s3_class(new_geos_geometry(list(NULL)), "geos_geometry")
  expect_error(new_geos_geometry(NULL), "must be a bare list")
  expect_true(is.na(new_geos_geometry(list(NULL))))
})

test_that("geos_geometry can be created from well-known text", {
  expect_is(as_geos_geometry("POINT (30 10)"), "geos_geometry")
  expect_length(as_geos_geometry("POINT (30 10)"), 1)
  expect_length(as_geos_geometry(c(NA, "POINT (30 10)")), 2)
  expect_match(format(as_geos_geometry("POINT (30 10)")), "^POINT")
  expect_output(print(as_geos_geometry("POINT (30 10)")), "POINT")
})

test_that("geos_geometry validation works", {
  expect_identical(validate_geos_geometry(new_geos_geometry(list())), new_geos_geometry(list()))
  expect_identical(validate_geos_geometry(new_geos_geometry(list(NULL))), new_geos_geometry(list(NULL)))
  expect_error(validate_geos_geometry(list("wrong type")), "must be externalptr")
})

test_that("geos_geometry subsetting and concatenation work", {
  geometry <- new_geos_geometry(list(NULL, NULL))
  expect_identical(geometry[1], new_geos_geometry(list(NULL)))
  expect_identical(geometry[[1]], geometry[1])
  expect_identical(c(geometry, geometry), new_geos_geometry(list(NULL, NULL, NULL, NULL)))
  expect_identical(rep(geometry, 2), new_geos_geometry(list(NULL, NULL, NULL, NULL)))
  expect_identical(rep_len(geometry, 4), new_geos_geometry(list(NULL, NULL, NULL, NULL)))
})

test_that("geos_geometry default print method works", {
  expect_output(print(new_geos_geometry()), "geos_geometry")
  expect_output(print(new_geos_geometry(list(NULL))), "geos_geometry")
})
