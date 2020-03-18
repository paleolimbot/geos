
test_that("wkt conversion works", {
  wkt <- geo_wkt(c("POINT (30 10)", "POINT (20 20)"))
  wkt_roundtrip <- geo_convert(wkt, new_geo_wkt())
  expect_is(wkt_roundtrip, "geo_wkt")
  expect_match(wkt_roundtrip, "^POINT")
  expect_match(wkt_roundtrip[1], c("\\(30\\."))
  expect_match(wkt_roundtrip[1], c("10\\.0+\\)"))
  expect_match(wkt_roundtrip[2], c("\\(20\\."))
})

test_that("wkb conversion works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  wkb <- geo_wkb(list(wkb_raw))
  wkb_roundtrip <- geo_convert(wkb, new_geo_wkb())
  expect_identical(wkb, wkb_roundtrip)
})

test_that("geo_coord_point conversion works", {
  tbl <- geo_convert(
    geo_wkt("POINT (10 40)"),
    geo_coord()
  )

  expect_identical(tbl, geo_coord_point(geo_xy(10, 40)))
})

test_that("geo_coord_linestring conversion works", {
  tbl <- geo_convert(
    geo_wkt("LINESTRING (30 10, 10 30, 40 40)"),
    geo_coord()
  )

  expect_identical(
    tbl,
    geo_coord_linestring(geo_xy(c(30, 10, 40), c(10, 30, 40)))
  )
})

test_that("geo_coord_multipoint conversion works", {
  tbl <- geo_convert(
    geo_wkt("MULTIPOINT ((10 40), (40 30))"),
    geo_coord()
  )

  expect_identical(
    tbl,
    geo_coord_multipoint(geo_xy(c(10, 40), c(40, 30)), feature = 1)
  )
})

test_that("geo_coord_multilinestring conversion works", {
  tbl <- geo_convert(
    geo_wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))"),
    geo_coord()
  )

  expect_identical(
    tbl,
    geo_coord_multilinestring(
      geo_xy(
        c(10, 20, 10, 40, 30, 40, 30),
        c(10, 20, 40, 40, 30, 20, 10)
      ),
      feature = 1,
      part = c(1, 1, 1, 2, 2, 2, 2)
    )
  )
})

test_that("geo_coord_polygon conversion works", {
  tbl <- geo_convert(
    geo_wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))"),
    geo_coord()
  )

  expect_identical(
    tbl,
    geo_coord_polygon(
      geo_xy(c(30, 10, 40, 30), c(10, 30, 40, 10))
    )
  )
})

test_that("geo_coord_multi_polygon conversion works", {
  tbl <- geo_convert(
    geo_wkt(
    "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),
            ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))
    "),
    geo_coord()
  )

  expect_identical(
    tbl,
    geo_coord_multipolygon(
      geo_xy(
        c(40, 20, 45, 40, 20, 10, 10, 30, 45, 20, 30, 20, 20, 30),
        c(40, 45, 30, 40, 35, 30, 10, 5,  20, 35, 20, 15, 25, 20)
      ),
      part = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
      piece = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2)
    )
  )
})

test_that("rect conversion works", {
  rect <- geo_convert(
    geo_wkt(
      "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),
            ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))
    "),
    geo_rect()
  )

  expect_identical(
    rect,
    geo_rect(xmin = 10, ymin = 5, xmax = 45, ymax = 45)
  )
})

test_that("xy conversion works", {
  expect_identical(
    geo_convert(geo_xy(1:5, 6:10), geo_coord()),
    geo_coord_point(geo_xy(1:5, 6:10))
  )

  # handling of NA, NaN (inf handled no prob, apparently)
  expect_equal(geo_convert(geo_xy(NA, NA), geo_wkt()), geo_wkt("POINT EMPTY"))
  expect_equal(geo_convert(geo_xy(NA, 1), geo_wkt()), geo_wkt("POINT EMPTY"))
  expect_equal(geo_convert(geo_xy(1, NA), geo_wkt()), geo_wkt("POINT EMPTY"))
  expect_equal(geo_convert(geo_xy(NaN, 1), geo_wkt()), geo_wkt("POINT EMPTY"))
  expect_equal(geo_convert(geo_xy(1, NaN), geo_wkt()), geo_wkt("POINT EMPTY"))
  expect_equal(geo_convert(geo_xy(NaN, NaN), geo_wkt()), geo_wkt("POINT EMPTY"))
})

test_that("empty geometrycollections can  be converted to a GeoCoord", {
  skip("geometrycollections not implemented but should be")
  geo_convert(geo_wkt("GEOMETRYCOLLECTION EMPTY"), geo_coord())
})

test_that("error occurs with unknown object in conversions", {
  expect_error(geomcpp_convert(NULL, new_geo_wkt()), "Can't resolve")
})

test_that("geo_buffer() returns the same format as the input by default", {
  expect_is(geo_buffer(geo_wkt("POINT (0 0)"), 1), "geo_wkt")
})

test_that("geo_buffer works", {
  point <- geo_wkt("POINT (0 0)")
  result <- geo_buffer(point, width = 1)
  expect_identical(
    geo_convert(result, geo_rect()),
    geo_rect(-1, -1, 1, 1)
  )
})

test_that("geo_buffer is vectorized along 'width'", {
  point <- geo_wkt(c("POINT (0 0)", "POINT (1 1)"))
  result1 <- geo_buffer(point, 1, quad_segs = 2)
  expect_length(result1, 2)

  expect_identical(
    geo_buffer(point, c(1, 2), quad_segs = 2),
    c(
      geo_buffer(point[1], 1, quad_segs = 2),
      geo_buffer(point[2], 2, quad_segs = 2)
    )
  )
})

test_that("geo_buffer works with all providers", {
  point <- geo_wkt("POINT (0 0)")
  buffered <- geo_buffer(point, 4)
  expect_identical(
    geo_buffer(geo_convert(point, geo_wkb()), 4, to = geo_wkt()),
    buffered
  )

  expect_identical(
    geo_buffer(geo_xy(0, 0), 4, to = geo_wkt()),
    buffered
  )
})
