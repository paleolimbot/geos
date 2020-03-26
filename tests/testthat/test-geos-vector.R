
test_that("segment intersection works", {
  expect_identical(
    geos_segment_intersection(
      geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
      geo_segment(geo_xy(10, 0), geo_xy(0, 10))
    ),
    geo_xy(5, 5)
  )
})
