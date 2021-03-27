
test_that("geos_geometry class works", {
  expect_s3_class(new_geos_geometry(list()), "geos_geometry")
  expect_s3_class(new_geos_geometry(list(NULL)), "geos_geometry")
  expect_error(new_geos_geometry(NULL), "must be a bare list")
  expect_true(is.na(new_geos_geometry(list(NULL))))
})

test_that("geos_geometry can be created from well-known text", {
  expect_is(as_geos_geometry("POINT (30 10)"), "geos_geometry")
  expect_is(as_geos_geometry(wk::wkt("POINT (30 10)")), "geos_geometry")
  expect_length(as_geos_geometry("POINT (30 10)"), 1)
  expect_length(as_geos_geometry(c(NA, "POINT (30 10)")), 2)
})

test_that("geos_geometry can be created from well-known binary", {
  expect_is(as_geos_geometry(structure(wk::as_wkb("POINT (30 10)"), class = "blob")), "geos_geometry")
  expect_is(as_geos_geometry(structure(wk::as_wkb("POINT (30 10)"), class = "WKB")), "geos_geometry")
})

test_that("geos_geometry validation works", {
  expect_identical(validate_geos_geometry(new_geos_geometry(list())), new_geos_geometry(list()))
  expect_identical(validate_geos_geometry(new_geos_geometry(list(NULL))), new_geos_geometry(list(NULL)))
  expect_error(validate_geos_geometry(list("wrong type")), "must be externalptr")
})

test_that("geos_geometry subsetting and concatenation work", {
  geometry <- new_geos_geometry(list(NULL, NULL))
  expect_identical(geometry[1], new_geos_geometry(list(NULL)))
  expect_identical(geometry[[1]], geometry[1])
  expect_identical(c(geometry, geometry), new_geos_geometry(list(NULL, NULL, NULL, NULL)))
  expect_identical(rep(geometry, 2), new_geos_geometry(list(NULL, NULL, NULL, NULL)))
  expect_identical(rep_len(geometry, 4), new_geos_geometry(list(NULL, NULL, NULL, NULL)))
  expect_error(c(new_geos_geometry(list(NULL, NULL)), 1:5), "must inherit from")
})

test_that("geos_geometry can be put into a data.frame", {
  expect_identical(
    data.frame(geom = new_geos_geometry(list(NULL))),
    new_data_frame(list(geom = new_geos_geometry(list(NULL))))
  )
})

test_that("geos_geometry default format/print/str methods work", {
  expect_identical(
    format(as_geos_geometry("POINT (10 10)")),
    as.character(as_geos_geometry("POINT (10 10)"))
  )
  expect_match(format(geos_make_linestring(1:6, 1)), "<LINESTRING \\[")
  expect_match(format(geos_make_linestring(1:5, 1)), "<LINESTRING \\(")
  expect_output(print(new_geos_geometry()), "geos_geometry")
  expect_output(print(new_geos_geometry(list(NULL))), "geos_geometry")
  expect_match(format(as_geos_geometry(c("POINT (0 1)", NA)))[2], "<NA>")

  expect_output(str(as_geos_geometry(character())), "geos_geometry\\[0\\]")
  expect_output(str(as_geos_geometry("POINT (10 10)")), "<POINT \\(10 10\\)>")
})
