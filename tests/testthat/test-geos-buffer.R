
test_that("geos_buffer() returns the same format as the input by default", {
  expect_is(geos_buffer(geo_wkt("POINT (0 0)"), 1), "geo_wkt")
})

test_that("geos_buffer works", {
  point <- geo_wkt("POINT (0 0)")
  result <- geos_buffer(point, width = 1)
  expect_identical(
    geo_convert(result, geo_rect()),
    geo_rect(-1, -1, 1, 1)
  )
})

test_that("geos_buffer is vectorized along 'width'", {
  point <- geo_wkt(c("POINT (0 0)", "POINT (1 1)"))
  result1 <- geos_buffer(point, 1, quad_segs = 2)
  expect_length(result1, 2)

  expect_identical(
    geos_buffer(point, c(1, 2), quad_segs = 2),
    c(
      geos_buffer(point[1], 1, quad_segs = 2),
      geos_buffer(point[2], 2, quad_segs = 2)
    )
  )

  expect_identical(
    geos_buffer(point[1], c(1, 2), quad_segs = 2),
    c(
      geos_buffer(point[1], 1, quad_segs = 2),
      geos_buffer(point[1], 2, quad_segs = 2)
    )
  )
})

test_that("geos_buffer works with all providers", {
  point <- geo_wkt("POINT (0 0)")
  buffered <- geos_buffer(point, 4)
  expect_identical(
    geos_buffer(geo_convert(point, geo_wkb()), 4, to = geo_wkt()),
    buffered
  )

  expect_identical(
    geos_buffer(geo_xy(0, 0), 4, to = geo_wkt()),
    buffered
  )
})
