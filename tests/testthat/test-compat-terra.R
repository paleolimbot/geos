test_that("conversion from terra works", {
  skip_if_not_installed("terra")

  polygon <- terra::vect("POLYGON ((0 -5, 10 0, 10 -10, 0 -5))")
  point <- terra::vect("POINT (0 -5)")
  empty <- terra::vect("POINT EMPTY")
  linestring <- terra::vect("LINESTRING (0 0, 1 1)")
  polygonz <- terra::vect("POLYGON Z ((0 0 0, 1 1 1, 2 2 2, 0 0 0))")

  expect_identical(
    geos_write_wkt(as_geos_geometry(polygon)),
    "POLYGON ((0 -5, 10 0, 10 -10, 0 -5))"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(point)),
    "POINT (0 -5)"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(empty)),
    "POINT EMPTY"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(linestring)),
    "LINESTRING (0 0, 1 1)"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(polygonz)),
    terra::geom(polygonz, wkt = TRUE)
  )
})

test_that("conversion from terra propagates CRS", {
  skip_if_not_installed("terra")
  polygon <- terra::vect(
    "POLYGON ((0 -5, 10 0, 10 -10, 0 -5))",
    crs = "epsg:4326"
  )
  polygon_geos <- as_geos_geometry(polygon)

  expect_identical(
    terra::crs(polygon, describe = TRUE)$name,
    wk::wk_crs(polygon_geos)$input
  )
})

test_that("as_geos_geometry from terra behaves identical to st_as_sf from sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  polygon <- terra::vect(
    "POLYGON ((0 -5, 10 0, 10 -10, 0 -5))",
    crs = "epsg:4326"
  )
  polygon_sf <- sf::st_as_sf(polygon)
  polygon_geos <- as_geos_geometry(polygon)
  expect_identical(sf::st_as_sf(polygon_geos), polygon_sf)
})
