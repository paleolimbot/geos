
test_that("transformers work", {
  expect_identical(
    geos_write_wkt(geos_centroid(c("POINT (0 1)", "LINESTRING (0 0, 1 1)", NA))),
    c("POINT (0 1)", "POINT (0.5 0.5)", NA)
  )
})
