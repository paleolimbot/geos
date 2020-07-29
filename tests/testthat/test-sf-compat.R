
test_that("conversion from sf works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)))
  expect_identical(
    geos_write_wkt(as_geos_geometry(sfc)),
    c("POINT EMPTY", "POINT (0 1)")
  )

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(
    geos_write_wkt(as_geos_geometry(sf)),
    c("POINT EMPTY", "POINT (0 1)")
  )
})

test_that("conversion to sf works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)))
  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  geom <- as_geos_geometry(c("POINT EMPTY", "POINT (0 1)"))

  expect_equal(sf::st_as_sf(geom), sf)
  expect_equal(sf::st_as_sfc(geom), sfc)
})
