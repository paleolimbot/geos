
test_that("atomic extractors work", {
  expect_identical(
    geos_area(c("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", NA)),
    c(100, NA)
  )
})
