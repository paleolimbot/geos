
test_that("unary predicates work", {
  expect_true(geos_is_empty(geo_wkt("POINT EMPTY")))
  expect_false(geos_is_empty(geo_wkt("POINT (30 10)")))

  # is self-intersecting
  expect_false(geos_is_simple(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0, 10 10)")))
  expect_true(geos_is_simple(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0)")))

  expect_true(geos_has_z(geo_wkt("POINT (10 10 1)")))
  expect_false(geos_has_z(geo_wkt("POINT (10 10)")))

  expect_true(geos_is_closed(geo_wkt("LINESTRING (0 0, 0 10, 10 0, 0 0)")))
  expect_false(geos_is_closed(geo_wkt("LINESTRING (0 0, 0 10, 10 0)")))
})

test_that("geom type extractors work", {
  expect_identical(geos_geom_type_id(geo_wkt("POINT EMPTY")), 0L)
  expect_identical(geos_geom_type_id(geo_wkt("LINESTRING EMPTY")), 1L)
  expect_identical(geos_geom_type_id(geo_wkt("POLYGON EMPTY")), 3L)
  expect_identical(geos_geom_type_id(geo_wkt("MULTIPOINT EMPTY")), 4L)
  expect_identical(geos_geom_type_id(geo_wkt("MULTILINESTRING EMPTY")), 5L)
  expect_identical(geos_geom_type_id(geo_wkt("MULTIPOLYGON EMPTY")), 6L)
  expect_identical(geos_geom_type_id(geo_wkt("GEOMETRYCOLLECTION EMPTY")), 7L)

  expect_identical(geos_geom_type(geo_wkt("POINT EMPTY")), "point")
  expect_identical(geos_geom_type(geo_wkt("LINESTRING EMPTY")), "linestring")
  expect_identical(geos_geom_type(geo_wkt("POLYGON EMPTY")), "polygon")
  expect_identical(geos_geom_type(geo_wkt("MULTIPOINT EMPTY")), "multipoint")
  expect_identical(geos_geom_type(geo_wkt("MULTILINESTRING EMPTY")), "multilinestring")
  expect_identical(geos_geom_type(geo_wkt("MULTIPOLYGON EMPTY")), "multipolygon")
  expect_identical(geos_geom_type(geo_wkt("GEOMETRYCOLLECTION EMPTY")), "geometrycollection")
})

test_that("srid extractor works", {
  # POINT (0, 0) with epsg=4326 as EWKB
  # sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326) %>% sf:::CPL_write_wkb(EWKB = TRUE) %>% dput()
  wkb <- geo_wkb(
    list(
      as.raw(
        c(0x01, 0x01, 0x00, 0x00, 0x20, 0xe6, 0x10, 0x00,
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
      )
    )
  )

  expect_identical(geos_get_srid(wkb), 4326L)
})

test_that("geometry counters work",  {
  expect_equal(geos_n_geometries(geo_wkt("POINT EMPTY")), 1)
  expect_equal(geos_n_geometries(geo_wkt("POINT (0 0)")), 1)
  expect_equal(geos_n_geometries(geo_wkt("MULTIPOINT (0 0, 10 10)")), 2)

  expect_equal(geos_n_coordinates(geo_wkt("POINT EMPTY")), 0)
  expect_equal(geos_n_coordinates(geo_wkt("POINT (0 0)")), 1)
  expect_equal(geos_n_coordinates(geo_wkt("MULTIPOINT (0 0, 10 10)")), 2)

  expect_equal(geos_n_points(geo_wkt("LINESTRING EMPTY")), 0)
  expect_equal(geos_n_points(geo_wkt("LINESTRING (0 0, 10 10)")), 2)
  expect_equal(geos_n_points(geo_wkt("LINESTRING (0 0, 5 5, 10 10)")), 3)

  poly_hole <- geo_wkt("POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))")
  poly <- geo_wkt("POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10))")
  expect_equal(geos_n_interior_rings(geo_wkt("POLYGON EMPTY")), 0)
  expect_equal(geos_n_interior_rings(poly), 0)
  expect_equal(geos_n_interior_rings(poly_hole), 1)

  expect_equal(geos_n_dimensions(geo_wkt("POINT (10 10)")), 0)
  expect_equal(geos_n_dimensions(geo_wkt("LINESTRING (10 10, 0 0)")), 1)
  expect_equal(geos_n_dimensions(geo_wkt("POLYGON ((10 10, 0 0, 0 10, 10 10))")), 2)
  expect_equal(geos_n_dimensions(geo_wkt("GEOMETRYCOLLECTION(POINT (10 10))")), 0)
  expect_equal(geos_n_dimensions(geo_wkt("GEOMETRYCOLLECTION(LINESTRING (10 10, 0 0))")), 1)

  expect_equal(geos_n_coordinate_dimensions(geo_wkt("POINT (10 10)")), 2)
  expect_equal(geos_n_coordinate_dimensions(geo_wkt("POINT (10 10 0)")), 3)
})
