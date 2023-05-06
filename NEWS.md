# geos (development version)

* Fix NA/NaN issues for forthcoming waldo package update (@hadley, #83).
* Fix test on M1Mac (#85, #86).

# geos 0.2.2

* Fix new warnings from `-Wstrict-prototypes`.

# geos 0.2.1

* Fix address sanitizer error identified by the clang sanitizer.

# geos 0.2.0

* Added an experimental `geos_inner_join()` and `geos_inner_join_keys()` for
  potentially faster and/or more memory-efficient joins (#76, #66).
* Added the `geos_basic_strtree()` class and query methods to create tree
  indices from non-GEOS geometries while making fewer copies (#73, #66).
* Rewrote the GEOS-geometry construction code because of a bug that
  prevented it from being used for default conversion (#74, #56).
* Added support for new features in GEOS 3.11, including
  `geos_line_merge()`, `geos_line_merge_directed()`, `geos_concave_hull()`,
  `geos_remove_repeated_points()`, `geos_hilbert_code()`, and
  `geos_transform_xy()` (#69).
* Clarified documentation on make and unnest functions (#70, #68).

# geos 0.1.3

* Add support for new features in GEOS 3.10, including
  `geos_is_within_distance()`, `geos_prepared_is_within_distance()`,
  `geos_read_geojson()`, `geos_write_geojson()`,
  `geos_constrained_delaunay_triangles()`, `geos_densify()`,
  and a new argument `flavor` to `geos_write_wkb()` and
  `geos_write_hex()` to allow ISO WKB output instead of
  EWKB output for geometries with Z values (#57).
* Fix crash when EMPTY points were passed to
  `geos_project()` or `geos_project_normalized()` (#52, #61).
* Use `geos_simplify()`, `geos_strtree_query()`, and
  `geos_clip_by_rect()` to speed up plotting for polygons/
  lines with many vertices (#59, #61).
* Update tests to use testthat 3rd edition (#62).
* Add temporary workaround for intermittent crashes 
  when importing large objects using `geos_geometry_writer()`
  (workaround uses `sf::st_as_binary()` and `geos_read_wkb()`
  until the source of the crash can be determined) (#56).

# geos 0.1.2

* Update tests to pass on GEOS 3.9.1 and 3.10.0 (#54).

# geos 0.1.1

* Fixed an issue with the internals of the strtree object
  that resulted in a valgrind error on the CRAN check page.

# geos 0.1.0

* Added vector class improvements for `geos_geometry()` vectors
  including a proper `str()` method, proper behaviour with
  `data.frame()`, subset-assignment, and faster `is.na()`.
* `geos_geometry()` vectors now propagate CRS objects using
  the `wk::wk_crs()`/`wk::wk_crs_output()` system.
* Fixed a major bug in `geos_strtree()` whereby the `node_capacity` had
  been set equal to the length of the input.
* Added `geos_minimum_bounding_crc()` (like `geos_minimum_bounding_circle()`
  but returning a `wk::crc()`).
* Added `geos_envelope_rct()` (like `geos_envelope()` but returning
  a `wk::rct()`).
* Added `geos_maximum_inscribed_crc()` (like 
  `geos_maximum_inscribed_circle_spec()` but returning a `wk::crc()`).
* Added more comprehensive vctrs support including casting and
  concatenation with wk vector types.
* Added new functions available in GEOS 3.9.1:
  `geos_intersection_prec()`, `geos_difference_prec()`,
  `geos_sym_difference_prec()`, `geos_union_prec()`,
  `geos_unary_union_prec()`, `geos_largest_empty_circle()`,
  `geos_maximum_inscribed_circle_spec()`,
  `geos_prepared_nearest_points()`, 
  and `geos_prepared_distance()`. These functions are only
  available when compiled with libgeos >= 3.9.1.

# geos 0.0.2

* Dropped support for `wk::wksxp()`.

# geos 0.0.1

* Initial CRAN release.
