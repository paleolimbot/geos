
test_that("geos_inner_join() works", {
  x <- data.frame(
    col_x = "a",
    geometry = as_geos_geometry("POINT (10 10)")
  )

  y <- data.frame(
    col_y = "a",
    geometry = as_geos_geometry("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))")
  )

  geos_inner_join(x, y)
})

test_that("geos_inner_join_keys() works", {
  expect_keys_true <- function(x) {
    expect_identical({{ x }}, data.frame(x = 1L, y = 1L))
  }

  expect_keys_false <- function(x) {
    expect_identical({{ x }}, data.frame(x = integer(), y = integer()))
  }

  expect_keys_true(
    geos_inner_join_keys(
      "POINT (10 10)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "touches"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "intersects"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "LINESTRING (-1 -1, 6 6)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "crosses"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "within"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)",
      "contains"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)",
      "contains_properly"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POLYGON ((1 1, 1 11, 11 11, 11 1, 1 1))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "overlaps"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "equals"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "equals_exact",
      distance = 0
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      "equals_exact",
      distance = 0.2
    )
  )

  expect_keys_false(
    geos_inner_join_keys(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POLYGON ((0.1 0.1, 0 10, 10 10, 10 0, 0.1 0.1))",
      "equals_exact",
      distance = 0.05
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "POINT (5 5)",
      "covers"
    )
  )

  expect_keys_true(
    geos_inner_join_keys(
      "POINT (5 5)",
      "POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))",
      "covered_by"
    )
  )
})
