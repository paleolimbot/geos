
test_that("geos_basic_strtree objects can be created", {
  tree <- geos_basic_strtree()
  expect_s3_class(tree, "geos_basic_strtree")
  expect_false(geos_basic_strtree_finalized(tree))
  expect_identical(geos_basic_strtree_size(tree), 0L)
})
