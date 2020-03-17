
test_that("intersection works", {
  result <- geo_intersection(
    geo_wkt("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))"),
    geo_wkt("POLYGON ((5 5, 5 15, 10 15, 15 5, 5 5))"),
    to = geo_tbl()
  )

  expect_identical(range(field(field(result, "xy"), "x")), c(5, 10))
  expect_identical(range(field(field(result, "xy"), "y")), c(5, 10))
})
