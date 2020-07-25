
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

test_that("strtree objects that are invalid cannot be queried", {
  tree <- geos_strtree("POINT (30 10)")

  temprds <- tempfile()
  saveRDS(tree, temprds)
  tree <- readRDS(temprds)
  expect_error(geos_strtree_query(tree, "POINT (30 10)"), "External.*?is not valid")
})

test_that("empty trees can be queried", {
  expect_identical(
    geos_strtree_query(character(0), c("POINT (30 10)", "POINT (0 0)")),
    list(integer(), integer())
  )
})

test_that("strtree objects can be queried", {
  tree <- geos_strtree(
    c("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", "POLYGON ((0 0, 0 -10, -10 -10, -10 0, 0 0))")
  )

  expect_identical(
    lapply(
      geos_strtree_query(
        tree,
        c("POINT (-5 -5)", "POINT (5 5)", "MULTIPOINT (-5 -5, 5 5)", NA)
      ),
      sort
    ),
    list(2L, 1L, c(1L, 2L), NULL)
  )
})

test_that("matrix predicates return the correct shape output", {
  tree <- geos_strtree(
    c("POLYGON ((0 0, 10 0, 0 10, 0 0))", "POLYGON ((0 0, 0 -10, -10 0, 0 0))")
  )

  expect_identical(
    lapply(
      geos_intersects_matrix(
        c("POINT (-2 -2)", "MULTIPOINT (-2 -2, 2 2)", "POINT (6 6)", "POINT (11 11)", NA),
        tree
      ),
      sort
    ),
    list(2L, c(1L, 2L), integer(), integer(), NULL)
  )
})

test_that("matrix predicates work", {

  expect_matrix_true <- function(x) expect_identical({{ x }}, list(1L))
  expect_matrix_false <- function(x) expect_identical({{ x }}, list(integer()))

  expect_matrix_false(
    geos_disjoint_matrix(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_touches_matrix(
      "POINT (10 10)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_intersects_matrix(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_crosses_matrix(
      "LINESTRING (-1 -1, 6 6)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_within_matrix(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_contains_matrix(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_matrix_true(
    geos_contains_properly_matrix(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_matrix_true(
    geos_overlaps_matrix(
      "POLYGON ((1 1, 1 11, 11 11, 11 1, 1 1))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_equals_matrix(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_equals_exact_matrix(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_matrix_true(
    geos_equals_exact_matrix(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      tolerance = 0.2
    )
  )

  expect_matrix_false(
    geos_equals_exact_matrix(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      tolerance = 0.05
    )
  )

  expect_matrix_true(
    geos_covers_matrix(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_matrix_true(
    geos_covered_by_matrix(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )
})

test_that("_any() predicates work", {
  # check NA
  expect_identical(
    geos_disjoint_any(NA_character_, "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"),
    NA
  )

  expect_false(
    geos_disjoint_any(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_touches_any(
      "POINT (10 10)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_intersects_any(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_crosses_any(
      "LINESTRING (-1 -1, 6 6)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_within_any(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_contains_any(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_contains_properly_any(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_overlaps_any(
      "POLYGON ((1 1, 1 11, 11 11, 11 1, 1 1))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_equals_any(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_equals_exact_any(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )

  expect_true(
    geos_equals_exact_any(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      tolerance = 0.2
    )
  )

  expect_false(
    geos_equals_exact_any(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      tolerance = 0.05
    )
  )

  expect_true(
    geos_covers_any(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)"
    )
  )

  expect_true(
    geos_covered_by_any(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"
    )
  )
})

test_that("nearest functions work", {
  expect_identical(
    geos_nearest(
      c(NA, "POINT (0.9 0.9)", "POINT (0.1 0.1)"),
      c("POINT (0 0)", "POINT (1 1)")
    ),
    c(NA, 2L, 1L)
  )

  expect_identical(
    geos_nearest_indexed(
      c(NA, "POINT (0.9 0.9)", "POINT (0.1 0.1)"),
      c("POINT (0 0)", "POINT (1 1)")
    ),
    c(NA, 2L, 1L)
  )

  expect_identical(
    geos_nearest_hausdorff(
      c(NA, "POINT (0.9 0.9)", "POINT (0.1 0.1)"),
      c("POINT (0 0)", "POINT (1 1)")
    ),
    c(NA, 2L, 1L)
  )

  expect_identical(
    geos_nearest_hausdorff(
      c(NA, "POINT (0.9 0.9)", "POINT (0.1 0.1)"),
      c("POINT (0 0)", "POINT (1 1)"),
      densify = 0.5
    ),
    c(NA, 2L, 1L)
  )

  expect_identical(
    geos_nearest_frechet(
      c(NA, "POINT (0.9 0.9)", "POINT (0.1 0.1)"),
      c("LINESTRING (0 0, -1 -1)", "LINESTRING (1 1, 2 2)"),
    ),
    c(NA, 2L, 1L)
  )

  expect_identical(
    geos_nearest_frechet(
      c(NA, "POINT (0.9 0.9)", "POINT (0.1 0.1)"),
      c("LINESTRING (0 0, -1 -1)", "LINESTRING (1 1, 2 2)"),
      densify = 0.5
    ),
    c(NA, 2L, 1L)
  )

  # empty tree
  expect_identical(
    geos_nearest(c(NA, "POINT (0.9 0.9)", "POINT (0.1 0.1)"), character()),
    c(NA_integer_, NA_integer_, NA_integer_)
  )

  # invalid tree
  bad_ptr <- geos_strtree("POINT (0 0)")
  tmp <- tempfile()
  saveRDS(bad_ptr, tmp)
  bad_ptr <- readRDS(tmp)
  expect_error(geos_nearest("POINT (0 0)", bad_ptr), "is not valid")

  # internal error
  expect_error(geos_nearest_error("POINT (0 0)", "POINT (0 0)"), "Failed to compute distance")
})
