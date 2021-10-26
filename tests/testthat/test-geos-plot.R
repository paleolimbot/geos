
test_that("ploting works", {
  expect_identical(
    plot(geos_geometry(), bbox = wk::rct(0, 0, 1, 1)),
    geos_geometry()
  )

  geom <- as_geos_geometry("LINESTRING (0 0, 1 1)")
  expect_identical(plot(geom), geom)

  geom2 <- as_geos_geometry(wk::rct(0, 0, 0.4, 0.4))
  expect_identical(plot(geom2, border = "red", col = "grey80", add = TRUE), geom2)

  # make sure clip/select operations don't separate vectorized par
  # (expecting green and brown points)
  geom3 <- as_geos_geometry(wk::xy(c(-10, 0.5, 10, 1), c(-10, 0.5, 10, 1)))
  plot(geom3, pch = 16, col = c("blue", "green", "purple", "brown"), add = TRUE)
})
