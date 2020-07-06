
test_that("segment intersection works", {
  expect_identical(
    geos_segment_intersection(
      list(0, 0, 10, 4),
      list(0, 4, 10, 0)
    ),
    list(x = 5, y = 2)
  )

  expect_identical(
    geos_segment_intersection(
      list(NA, 0, 10, 10),
      list(10, 0, NA, 10)
    ),
    list(x = NA_real_, y = NA_real_)
  )
})

test_that("orientation index works", {
  expect_identical(
    geos_orientation_index(
      list(0, 0, 10, 10),
      list(10, 20)
    ),
    1L
  )

  expect_identical(
    geos_orientation_index(
      list(0, 0, 10, 10),
      list(20, 10)
    ),
    -1L
  )

  expect_identical(
    geos_orientation_index(
      list(0, 0, 10, 10),
      list(20, 20)
    ),
    0L
  )

  expect_identical(
    geos_orientation_index(
      list(NA, NA, NA, NA),
      list(NA, 20)
    ),
    NA_integer_
  )
})
