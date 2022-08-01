
test_that("geos_basic_strtree objects can be created", {
  tree <- geos_basic_strtree()
  expect_s3_class(tree, "geos_basic_strtree")
  expect_false(geos_basic_strtree_finalized(tree))
  expect_identical(geos_basic_strtree_size(tree), 0L)
})

test_that("can insert geos_geometry to geos_basic_strtree", {
  tree <- geos_basic_strtree()

  geos_basic_strtree_insert(tree, geos_geometry())
  expect_identical(geos_basic_strtree_size(tree), 0L)

  expect_identical(
    geos_basic_strtree_insert(
      tree,
      as_geos_geometry(c("POINT (0 1)", "POINT (2 3)"))
    ),
    1:2
  )

  expect_identical(
    geos_basic_strtree_insert(
      tree,
      as_geos_geometry(c("POINT (0 1)", "POINT (2 3)"))
    ),
    3:4
  )
})

test_that("can insert non-geos_geometry to geos_basic_strtree", {
  tree <- geos_basic_strtree()

  geos_basic_strtree_insert(tree, wk::wkt())
  expect_identical(geos_basic_strtree_size(tree), 0L)

  expect_identical(
    geos_basic_strtree_insert(
      tree,
      wk::wkt(c("POINT (0 1)", "POINT (2 3)"))
    ),
    1:2
  )

  expect_identical(
    geos_basic_strtree_insert(
      tree,
      wk::wkt(c("POINT (0 1)", "POINT (2 3)"))
    ),
    3:4
  )
})
