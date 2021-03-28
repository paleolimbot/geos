
test_that("geos_geometry is a vctr", {
  skip_if_not_installed("vctrs")

  x <- new_geos_geometry(list(NULL), crs = NULL)
  expect_true(vctrs::vec_is(x))
  expect_identical(vctrs::vec_data(x), list(NULL))
  expect_identical(vctrs::vec_restore(list(NULL), x), x)
  expect_identical(vctrs::vec_ptype_abbr(x), "geos_geom")
})

test_that("cast/ptype2 implementations work", {
  expect_identical(vctrs::vec_cast(geos_geometry(), geos_geometry()), geos_geometry())

  expect_identical(vctrs::vec_cast(wk::wkt(), geos_geometry()), geos_geometry())
  expect_identical(vctrs::vec_cast(wk::wkb(), geos_geometry()), geos_geometry())
  expect_identical(vctrs::vec_cast(wk::xy(), geos_geometry()), geos_geometry())
  expect_identical(vctrs::vec_cast(wk::xyz(), geos_geometry()), geos_geometry())
  expect_identical(vctrs::vec_cast(wk::rct(), geos_geometry()), geos_geometry())
  expect_identical(vctrs::vec_cast(wk::crc(), geos_geometry()), geos_geometry())

  expect_identical(vctrs::vec_cast(geos_geometry(), wk::wkt()), wk::wkt())
  expect_identical(vctrs::vec_cast(geos_geometry(), wk::wkb()), wk::wkb())
  expect_identical(vctrs::vec_cast(geos_geometry(), wk::xy()), wk::xy())
  expect_identical(vctrs::vec_cast(geos_geometry(), wk::xyz()), wk::xyz())

  expect_identical(vctrs::vec_c(geos_geometry(), geos_geometry()), geos_geometry())
  expect_identical(vctrs::vec_c(geos_geometry(), wk::wkb()), geos_geometry())
  expect_identical(vctrs::vec_c(geos_geometry(), wk::wkt()), geos_geometry())
  expect_identical(vctrs::vec_c(geos_geometry(), wk::xy()), geos_geometry())
  expect_identical(vctrs::vec_c(geos_geometry(), wk::xyz()), geos_geometry())
  expect_identical(vctrs::vec_c(geos_geometry(), wk::rct()), geos_geometry())
  expect_identical(vctrs::vec_c(geos_geometry(), wk::crc()), geos_geometry())
})

test_that("vec_c() propagates the crs attribute", {
  expect_identical(
    vctrs::vec_c(!!new_geos_geometry(crs = 1234), !!new_geos_geometry(crs = 1234)),
    !!new_geos_geometry(crs = 1234)
  )
  expect_identical(
    vctrs::vec_c(!!new_geos_geometry(crs = 1234), !!new_geos_geometry(crs = wk::wk_crs_inherit())),
    !!new_geos_geometry(crs = 1234)
  )
  expect_error(
    vctrs::vec_c(!!new_geos_geometry(crs = 1234), !!new_geos_geometry(crs = NULL)),
    "are not equal"
  )
})
