
test_that("segment intersection works", {
  expect_identical(
    geos_segment_intersection(
      geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
      geo_segment(geo_xy(10, 0), geo_xy(0, 10))
    ),
    geo_xy(5, 5)
  )

  expect_identical(
    geos_segment_intersection(
      geo_segment(geo_xy(NA, 0), geo_xy(10, 10)),
      geo_segment(geo_xy(10, 0), geo_xy(NA, 10))
    ),
    geo_xy(NA, NA)
  )
})

test_that("orientation index works", {
  expect_identical(
    geos_orientation_index(
      geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
      geo_xy(10, 20)
    ),
    1L
  )

  expect_identical(
    geos_orientation_index(
      geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
      geo_xy(20, 10)
    ),
    -1L
  )

  expect_identical(
    geos_orientation_index(
      geo_segment(geo_xy(0, 0), geo_xy(10, 10)),
      geo_xy(20, 20)
    ),
    0L
  )

  expect_identical(
    geos_orientation_index(
      geo_segment(geo_xy(NA, NA), geo_xy(NA, NA)),
      geo_xy(NA, 20)
    ),
    NA_integer_
  )
})
