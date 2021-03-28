
test_that("geos_geometry is a vctr", {
  skip_if_not_installed("vctrs")

  x <- new_geos_geometry(list(NULL), crs = NULL)
  expect_true(vctrs::vec_is(x))
  expect_identical(vctrs::vec_data(x), list(NULL))
  expect_identical(vctrs::vec_restore(list(NULL), x), x)
  expect_identical(vctrs::vec_ptype_abbr(x), "geos_geom")
})
