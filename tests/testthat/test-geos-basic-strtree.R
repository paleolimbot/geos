
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

test_that("geos_basic_strtree can be queried", {
  tree <- geos_basic_strtree()
  geos_basic_strtree_insert(tree, wk::wkt(c("POINT (1 1)", "POINT (3 3)")))
  geos_basic_strtree_query(tree, wk::rct(0, 0, 2, 2))

  expect_true(geos_basic_strtree_finalized(tree))

  expect_identical(
    geos_basic_strtree_query(tree, wk::rct(0, 0, 2, 2)),
    data.frame(x = 1L, tree = 1L)
  )

  expect_identical(
    geos_basic_strtree_query(tree, wk::rct(2, 2, 4, 4)),
    data.frame(x = 1L, tree = 2L)
  )

  expect_identical(
    geos_basic_strtree_query(tree, as_geos_geometry(NA_character_)),
    data.frame(x = integer(), tree = integer())
  )
})
