
test_that("strtree objects can be created", {
  expect_error(geos_strtree(NA_character_), "Can't insert NULL")

  geom <- geos_read_wkt("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  tree <- geos_strtree(geom)

  # make sure geom can be re-extracted from the object
  expect_reference(geos_strtree_data(tree), geom)

  # ...even  when it goes out of scope
  geom <- NULL
  expect_identical(
    geos_write_wkt(geos_strtree_data(tree)),
    "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))"
  )

  # and when the tree goes out of scope
  tree <- NULL
  gc()
})

test_that("strtree objects can be created from well-known text", {
  expect_identical(
    geos_write_wkt(geos_strtree_data(as_geos_strtree("POINT (0 0)"))),
    "POINT (0 0)"
  )

  # check zero-length
  expect_identical(
    geos_write_wkt(geos_strtree_data(as_geos_strtree(character(0)))),
    character(0)
  )
})

test_that("strtree objects have reasonable format() and print() methods", {
  expect_identical(format(geos_strtree(character(0))), "<geos_strtree containing 0 items>")
  expect_output(print(geos_strtree(character(0))), "<geos_strtree containing 0 items>")
})
