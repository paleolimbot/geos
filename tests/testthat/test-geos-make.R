
test_that("make point works", {
  expect_true(geos_is_empty(geos_make_point(NA, NA)))
  expect_identical(geos_write_wkt(geos_make_point(1, 2)), "POINT (1 2)")
  expect_identical(geos_write_wkt(geos_make_point(1, 2, 3)), "POINT Z (1 2 3)")
})

test_that("make linestring works", {
  expect_true(geos_is_empty(geos_make_linestring(double(), double(), double())))
  expect_identical(
    geos_write_wkt(geos_make_linestring(1:2, 3:4)),
    "LINESTRING (1 3, 2 4)"
  )
  expect_identical(
    geos_write_wkt(geos_make_linestring(1:2, 3:4, 5:6)),
    "LINESTRING Z (1 3 5, 2 4 6)"
  )
  expect_error(geos_make_linestring(1, 1), "IllegalArgumentException")
})

test_that("make collection works", {
  expect_identical(
    geos_write_wkt(
      geos_make_collection(c("POINT (1 1)", "POINT (2 2)"), feature_id = 1L)
    ),
    "GEOMETRYCOLLECTION (POINT (1 1), POINT (2 2))"
  )
  expect_identical(
    geos_write_wkt(
      geos_make_collection(c("POINT (1 1)", "POINT (2 2)"), feature_id = 1:2)
    ),
    c("GEOMETRYCOLLECTION (POINT (1 1))", "GEOMETRYCOLLECTION (POINT (2 2))")
  )

  expect_identical(geos_write_wkt(geos_make_collection(character(0))), "GEOMETRYCOLLECTION EMPTY")

  expect_error(geos_make_collection(c("POINT (1 1)", NA)), "Can't nest a missing")
  expect_error(geos_make_collection("POINT (1 2)", type_id = 12), "Unsupported type")

  bad_ptr <- geos_read_wkt("POINT (1 1)")
  tmp <- tempfile()
  saveRDS(bad_ptr, tmp)
  bad_ptr <- readRDS(tmp)
  expect_error(geos_make_collection(bad_ptr), "External pointer is not valid")
})
