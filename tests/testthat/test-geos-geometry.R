
test_that("geos_geometry class works", {
  expect_s3_class(new_geos_geometry(list(), crs = NULL), "geos_geometry")
  expect_s3_class(new_geos_geometry(list(NULL), crs = NULL), "geos_geometry")
  expect_error(new_geos_geometry(NULL, crs = NULL), "must be a bare list")
  expect_true(is.na(new_geos_geometry(list(NULL), crs = NULL)))
})

test_that("geos_geometry can be created from well-known text", {
  expect_s3_class(as_geos_geometry("POINT (30 10)"), "geos_geometry")
  expect_s3_class(as_geos_geometry(wk::wkt("POINT (30 10)")), "geos_geometry")
  expect_length(as_geos_geometry("POINT (30 10)"), 1)
  expect_length(as_geos_geometry(c(NA, "POINT (30 10)")), 2)
})

test_that("geos_geometry can be created from well-known binary", {
  expect_s3_class(as_geos_geometry(structure(wk::as_wkb("POINT (30 10)"), class = "blob")), "geos_geometry")
  expect_s3_class(as_geos_geometry(structure(wk::as_wkb("POINT (30 10)"), class = "WKB")), "geos_geometry")
})

test_that("geos_geometry validation works", {
  expect_identical(
    validate_geos_geometry(new_geos_geometry(list(), crs = NULL)),
    new_geos_geometry(list(), crs = NULL)
  )
  expect_identical(
    validate_geos_geometry(new_geos_geometry(list(NULL), crs = NULL)),
    new_geos_geometry(list(NULL), crs = NULL)
  )
  expect_error(validate_geos_geometry(list("wrong type")), "must be externalptr")
})

test_that("geos_geometry subsetting and concatenation work", {
  geometry <- new_geos_geometry(list(NULL, NULL), crs = NULL)
  expect_identical(geometry[1], new_geos_geometry(list(NULL), crs = NULL))
  expect_identical(geometry[[1]], geometry[1])
  expect_identical(
    c(geometry, geometry),
    new_geos_geometry(list(NULL, NULL, NULL, NULL), crs = NULL)
  )
  expect_identical(
    rep(geometry, 2),
    new_geos_geometry(list(NULL, NULL, NULL, NULL), crs = NULL)
  )
  expect_identical(
    rep_len(geometry, 4),
    new_geos_geometry(list(NULL, NULL, NULL, NULL), crs = NULL)
  )
  expect_error(
    c(new_geos_geometry(list(NULL, NULL), crs = NULL), 1:5),
    "must inherit from"
  )
})

test_that("subset-assignment works", {
  x <- as_geos_geometry(c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY"))
  expect_identical(geos_write_wkt(x[3]), "POLYGON EMPTY")
  x[3] <- "POINT (1 2)"
  expect_identical(geos_write_wkt(x[3]), "POINT (1 2)")
  x[3] <- as_geos_geometry("POINT (3 4)", crs = wk::wk_crs_inherit())
  expect_identical(geos_write_wkt(x[3]), "POINT (3 4)")

  x[[3]] <- "POINT (1 2)"
  expect_identical(geos_write_wkt(x[3]), "POINT (1 2)")

  expect_error(x[3] <- as_geos_geometry("POINT (1 2)", crs = 123), "are not equal")
})

test_that("geos_geometry can be put into a data.frame", {
  expect_identical(
    data.frame(geom = new_geos_geometry(list(NULL), crs = NULL)),
    new_data_frame(list(geom = new_geos_geometry(list(NULL), crs = NULL)))
  )
  expect_error(as.data.frame(new_geos_geometry(list(NULL), crs = NULL)), "cannot coerce class")
})

test_that("geos_geometry default format/print/str methods work", {
  expect_identical(
    format(as_geos_geometry("POINT (10 10)")),
    as.character(as_geos_geometry("POINT (10 10)"))
  )
  expect_match(format(geos_make_linestring(1:6, 1)), "<LINESTRING \\[")
  expect_match(format(geos_make_linestring(1:5, 1)), "<LINESTRING \\(")
  expect_output(print(new_geos_geometry(crs = NULL)), "geos_geometry")
  expect_output(print(new_geos_geometry(list(NULL), crs = NULL)), "geos_geometry")
  expect_output(print(new_geos_geometry(crs = 1234)), "CRS=1234")
  expect_match(format(as_geos_geometry(c("POINT (0 1)", NA)))[2], "<NA>")

  expect_output(str(as_geos_geometry(character())), "geos_geometry\\[0\\]")
  expect_output(str(as_geos_geometry("POINT (10 10)")), "<POINT \\(10 10\\)>")
})
