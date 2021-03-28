
test_that("make point works", {
  expect_true(geos_is_empty(geos_make_point(NA, NA)))
  expect_identical(geos_write_wkt(geos_make_point(1, 2)), "POINT (1 2)")
  expect_identical(geos_write_wkt(geos_make_point(1, 2, 3)), "POINT Z (1 2 3)")
  expect_identical(wk::wk_crs(geos_make_point(1, 2, crs = 123)), 123)
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
  expect_identical(wk::wk_crs(geos_make_linestring(1:2, 2:3, crs = 123)), 123)
})

test_that("make polygon works", {
  expect_true(geos_is_empty(geos_make_polygon(double(), double(), double())))

  # no hole
  expect_identical(
    geos_write_wkt(geos_make_polygon(c(0, 1, 0, 0), c(0, 0, 1, 0))),
    "POLYGON ((0 0, 1 0, 0 1, 0 0))"
  )

  # with z
  expect_identical(
    geos_write_wkt(geos_make_polygon(c(0, 1, 0, 0), c(0, 0, 1, 0), 5)),
    "POLYGON Z ((0 0 5, 1 0 5, 0 1 5, 0 0 5))"
  )

  # with hole
  expect_identical(
    geos_write_wkt(
      geos_make_polygon(
        c(0, 1, 0, 0, 0.1, 0.2, 0.1, 0.1),
        c(0, 0, 1, 0, 0.1, 0.1, 0.2, 0.1),
        ring_id = rep(1:2, each = 4)
      )
    ),
    "POLYGON ((0 0, 1 0, 0 1, 0 0), (0.1 0.1, 0.2 0.1, 0.1 0.2, 0.1 0.1))"
  )

  # multiple features
  expect_identical(
    geos_write_wkt(
      geos_make_polygon(
        c(0, 1, 0, 0, 0.1, 0.2, 0.1, 0.1),
        c(0, 0, 1, 0, 0.1, 0.1, 0.2, 0.1),
        feature_id = rep(1:2, each = 4)
      )
    ),
    c("POLYGON ((0 0, 1 0, 0 1, 0 0))", "POLYGON ((0.1 0.1, 0.2 0.1, 0.1 0.2, 0.1 0.1))")
  )

  # auto-closing hole (2D)
  expect_identical(
    geos_write_wkt(geos_make_polygon(c(0, 1, 0), c(0, 0, 1))),
    "POLYGON ((0 0, 1 0, 0 1, 0 0))"
  )

  # auto-closing hole (3D)
  expect_identical(
    geos_write_wkt(geos_make_polygon(c(0, 1, 0), c(0, 0, 1), 5)),
    "POLYGON Z ((0 0 5, 1 0 5, 0 1 5, 0 0 5))"
  )

  # bad ring (first and subsequent)
  expect_error(geos_make_polygon(1, 1), "IllegalArgumentException")
  expect_error(
    geos_make_polygon(c(0, 1, 0, 0, 12), c(0, 0, 1, 0, 12), ring_id = c(rep(1, 4), 2)),
    "IllegalArgumentException"
  )

  # crs
  expect_identical(
    wk::wk_crs(geos_make_polygon(c(0, 1, 0, 0), c(0, 0, 1, 0), crs = 123)),
    123
  )
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

  expect_identical(
    wk::wk_crs(geos_make_collection(as_geos_geometry("POINT (1 2)", crs = 123))),
    123
  )
})
