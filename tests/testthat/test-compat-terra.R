test_that("conversion from terra works", {
  skip_if_not_installed("terra")

  polygon <- terra::vect("POLYGON ((0 -5, 10 0, 10 -10, 0 -5))")
  point <- terra::vect("POINT (0 -5)")
  linestring <- terra::vect("LINESTRING (0 0, 1 1)")
  polygonz <- terra::vect("POLYGON Z ((0 0 0, 1 1 1, 2 2 2, 0 0 0))")
  multipoint <- terra::vect("MULTIPOINT ((0 0), (1 1), (2 2))")
  multipolygon <- terra::vect(
    "MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)), ((2 2, 3 2, 3 3, 2 2)))"
  )
  multilinestring <- terra::vect(
    "MULTILINESTRING ((0 0, 1 1), (2 2, 3 3))"
  )

  expect_identical(
    geos_write_wkt(as_geos_geometry(polygon)),
    "POLYGON ((0 -5, 10 0, 10 -10, 0 -5))"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(point)),
    "POINT (0 -5)"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(linestring)),
    "LINESTRING (0 0, 1 1)"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(polygonz)),
    terra::geom(polygonz, wkt = TRUE)
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(multipoint)),
    "MULTIPOINT (0 0, 1 1, 2 2)"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(multipolygon)),
    "MULTIPOLYGON (((0 0, 1 0, 1 1, 0 0)), ((2 2, 3 2, 3 3, 2 2)))"
  )
  expect_identical(
    geos_write_wkt(as_geos_geometry(multilinestring)),
    "MULTILINESTRING ((0 0, 1 1), (2 2, 3 3))"
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

  v <- terra::vect(system.file("ex/lux.shp", package = "terra"))
  v_geos <- as_geos_geometry(v)
  expect_identical(
    terra::crs(v, describe = TRUE)$name,
    wk::wk_crs(v_geos)$input
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
  polygon_geos <- sf::st_as_sf(as_geos_geometry(polygon))

  expect_identical(polygon_geos, polygon_sf)
  expect_identical(sf::st_crs(polygon_geos), sf::st_crs(polygon_sf))
})

test_that("conversion to terra works", {
  skip_if_not_installed("terra")

  pv <- terra::vect(
    "POLYGON ((0 -5, 10 0, 10 -10, 0 -5))",
    crs = "epsg:4326"
  )
  pvgv <- terra::vect(as_geos_geometry(pv))

  expect_s4_class(pvgv, "SpatVector")
  expect_identical(terra::geom(pvgv), terra::geom(pv))
  expect_identical(terra::geom(pvgv, wkt = TRUE), terra::geom(pv, wkt = TRUE))
  expect_identical(terra::crs(pv), terra::crs(pvgv))
})

test_that("conversion to terra works with numeric crs", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  expect_warning(
    terra::vect(geos_read_wkt("POINT (0 1)", crs = 4326))
  )
  testthat::expect_s4_class(
    terra::vect(geos_read_wkt("POINT (0 1)", crs = sf::st_crs(4326))),
    "SpatVector"
  )
})


# FIXME:
# Should be fixed upstream:
# {terra} erroreneously converts to WKB empty POINT
test_that("bug in terra", {
  empty <- terra::vect("POINT EMPTY")

  expect_identical(
    geos_write_wkt(as_geos_geometry(empty)),
    "MULTIPOINT EMPTY" # SHOULD BE "POINT EMPTY"
  )
})
