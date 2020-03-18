
test_that("unite_xy() works", {
  tbl <- tibble(x = 1, y = 2)
  expect_identical(
    unite_xy(tbl, "xy", x, y, remove = TRUE),
    tibble(xy = geo_xy(1, 2))
  )

  expect_identical(
    unite_xy(tbl, "xy", x, y, remove = FALSE),
    tibble(xy = geo_xy(1, 2), x = 1, y = 2)
  )

  tbl2 <- tibble(a = "unrelated", x = 1, y = 2)
  expect_identical(
    unite_xy(tbl2, "xy", x, y, remove = TRUE),
    tibble(a = "unrelated", xy = geo_xy(1, 2))
  )

  expect_identical(
    unite_xy(tbl2, "xy", x, y, remove = FALSE),
    tibble(a = "unrelated", xy = geo_xy(1, 2), x = 1, y = 2)
  )
})

test_that("separate_xy() works", {
  tbl <- tibble(xy = geo_xy(1, 2))
  expect_identical(separate_xy(tbl, xy, remove = TRUE), tibble(x = 1, y = 2))
  expect_identical(separate_xy(tbl, xy, remove = FALSE), tibble(x = 1, y = 2, xy = geo_xy(1, 2)))

  tbl2 <- tibble(a = "unrelated", xy = geo_xy(1, 2))
  expect_identical(
    separate_xy(tbl2, xy, remove = TRUE),
    tibble(a = "unrelated", x = 1, y = 2)
  )
  expect_identical(
    separate_xy(tbl2, xy, remove = FALSE),
    tibble(a = "unrelated", x = 1, y = 2, xy = geo_xy(1, 2))
  )
})
