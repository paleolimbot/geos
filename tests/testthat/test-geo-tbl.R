
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
