# geos (development version)

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
