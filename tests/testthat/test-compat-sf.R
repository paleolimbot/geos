
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

test_that("conversion from sf propagates CRS", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(c(0, 1)), crs = 4326)
  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))

  expect_identical(wk_crs(as_geos_geometry(sfc)), sf::st_crs(4326))
  expect_identical(wk_crs(as_geos_geometry(sf)), sf::st_crs(4326))
})

test_that("conversion to sf propagates CRS", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(c(0, 1)), crs = 4326)
  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))

  expect_identical(sf::st_as_sfc(geos_read_wkt("POINT (0 1)", crs = 4326)), sfc)
  expect_identical(sf::st_as_sf(geos_read_wkt("POINT (0 1)", crs = 4326)), sf)
})
