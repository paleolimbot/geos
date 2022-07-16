
#include "libgeos.h"
#include "wk-v1.h"
#include "geos-common.h"
#include <Rinternals.h>

/* generated by data-raw/make_callentries.R */
extern SEXP geos_c_distance(SEXP geom1, SEXP geom2);
extern SEXP geos_c_distance_indexed(SEXP geom1, SEXP geom2);
extern SEXP geos_c_distance_hausdorff(SEXP geom1, SEXP geom2);
extern SEXP geos_c_distance_frechet(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_distance(SEXP geom1, SEXP geom2);
extern SEXP geos_c_distance_hausdorff_densify(SEXP geom1, SEXP geom2, SEXP densifyFrac);
extern SEXP geos_c_distance_frechet_densify(SEXP geom1, SEXP geom2, SEXP densifyFrac);
extern SEXP geos_c_project(SEXP geom1, SEXP geom2);
extern SEXP geos_c_project_normalized(SEXP geom1, SEXP geom2);
extern SEXP geos_c_disjoint(SEXP geom1, SEXP geom2);
extern SEXP geos_c_touches(SEXP geom1, SEXP geom2);
extern SEXP geos_c_intersects(SEXP geom1, SEXP geom2);
extern SEXP geos_c_crosses(SEXP geom1, SEXP geom2);
extern SEXP geos_c_within(SEXP geom1, SEXP geom2);
extern SEXP geos_c_contains(SEXP geom1, SEXP geom2);
extern SEXP geos_c_overlaps(SEXP geom1, SEXP geom2);
extern SEXP geos_c_equals(SEXP geom1, SEXP geom2);
extern SEXP geos_c_covers(SEXP geom1, SEXP geom2);
extern SEXP geos_c_covered_by(SEXP geom1, SEXP geom2);
extern SEXP geos_c_equals_exact(SEXP geom1, SEXP geom2, SEXP tolerance);
extern SEXP geos_c_is_within_distance(SEXP geom1, SEXP geom2, SEXP tolerance);
extern SEXP geos_c_prepared_disjoint(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_touches(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_intersects(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_crosses(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_within(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_contains(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_contains_properly(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_overlaps(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_covers(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_covered_by(SEXP geom1, SEXP geom2);
extern SEXP geos_c_prepared_is_within_distance(SEXP geom1, SEXP geom2, SEXP tolerance);
extern SEXP geos_c_relate(SEXP geom1, SEXP geom2, SEXP boundaryNodeRule);
extern SEXP geos_c_relate_pattern_match(SEXP match, SEXP pattern);
extern SEXP geos_c_intersection(SEXP geom1, SEXP geom2);
extern SEXP geos_c_difference(SEXP geom1, SEXP geom2);
extern SEXP geos_c_sym_difference(SEXP geom1, SEXP geom2);
extern SEXP geos_c_union(SEXP geom1, SEXP geom2);
extern SEXP geos_c_shared_paths(SEXP geom1, SEXP geom2);
extern SEXP geos_c_snap(SEXP geom1, SEXP geom2, SEXP param);
extern SEXP geos_c_intersection_prec(SEXP geom1, SEXP geom2, SEXP param);
extern SEXP geos_c_difference_prec(SEXP geom1, SEXP geom2, SEXP param);
extern SEXP geos_c_sym_difference_prec(SEXP geom1, SEXP geom2, SEXP param);
extern SEXP geos_c_union_prec(SEXP geom1, SEXP geom2, SEXP param);
extern SEXP geos_c_largest_empty_circle(SEXP geom1, SEXP geom2, SEXP param);
extern SEXP geos_c_clearance_line_between(SEXP geom1, SEXP geom2, SEXP prepare);
extern SEXP geos_c_geos_geometry_is_null(SEXP geom);
extern SEXP geos_c_geos_geometry_is_null_or_xptr(SEXP geom);
extern SEXP geos_c_read_wkt(SEXP input);
extern SEXP geos_c_write_wkt(SEXP input, SEXP includeZ, SEXP precision, SEXP trim);
extern SEXP geos_c_read_geojson(SEXP input);
extern SEXP geos_c_write_geojson(SEXP input, SEXP indent);
extern SEXP geos_c_read_wkb(SEXP input);
extern SEXP geos_c_write_wkb(SEXP input, SEXP includeZ, SEXP includeSRID, SEXP endian, SEXP flavor);
extern SEXP geos_c_read_hex(SEXP input);
extern SEXP geos_c_write_hex(SEXP input, SEXP includeZ, SEXP includeSRID, SEXP endian, SEXP flavor);
extern SEXP geos_c_write_xy(SEXP input);
extern SEXP geos_c_make_point(SEXP x, SEXP y, SEXP z);
extern SEXP geos_c_make_linestring(SEXP x, SEXP y, SEXP z, SEXP featureLengths);
extern SEXP geos_c_make_polygon(SEXP x, SEXP y, SEXP z, SEXP ringLengthsByFeature);
extern SEXP geos_c_make_collection(SEXP geom, SEXP typeId, SEXP featureLengths);
extern SEXP geos_c_create_rectangle(SEXP xmin_sexp, SEXP ymin_sexp, SEXP xmax_sexp, SEXP ymax_sexp);
extern SEXP geos_c_empty(SEXP typeId);
extern SEXP geos_c_polygonize(SEXP collection);
extern SEXP geos_c_polygonize_valid(SEXP collection);
extern SEXP geos_c_polygonize_cut_edges(SEXP collection);
extern SEXP geos_c_polygonize_full(SEXP collection);
extern SEXP geos_c_segment_intersection(SEXP Sax0, SEXP Say0, SEXP Sax1, SEXP Say1, SEXP Sbx0, SEXP Sby0, SEXP Sbx1, SEXP Sby1);
extern SEXP geos_c_orientation_index(SEXP SAx, SEXP SAy, SEXP SBx, SEXP SBy, SEXP SPx, SEXP SPy);
extern SEXP geos_c_strtree_create(SEXP geom, SEXP node_capacity);
extern SEXP geos_c_strtree_data(SEXP treeExternalPtr);
extern SEXP geos_c_strtree_query(SEXP treeExternalPtr, SEXP geom);
extern SEXP geos_c_touches_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_intersects_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_crosses_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_within_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_contains_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_contains_properly_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_overlaps_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_covers_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_covered_by_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_equals_matrix(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_equals_exact_matrix(SEXP geom, SEXP treeExternalPtr, SEXP tolerance);
extern SEXP geos_c_predicate_any(SEXP matrixResult);
extern SEXP geos_c_nearest_error(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_nearest(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_nearest_indexed(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_nearest_hausdorff(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_nearest_frechet(SEXP geom, SEXP treeExternalPtr);
extern SEXP geos_c_nearest_frechet_densify(SEXP geom, SEXP treeExternalPtr, SEXP densify);
extern SEXP geos_c_nearest_hausdorff_densify(SEXP geom, SEXP treeExternalPtr, SEXP densify);
extern SEXP geos_c_area(SEXP geom);
extern SEXP geos_c_length(SEXP geom);
extern SEXP geos_c_x(SEXP geom);
extern SEXP geos_c_y(SEXP geom);
extern SEXP geos_c_z(SEXP geom);
extern SEXP geos_c_xmin(SEXP geom);
extern SEXP geos_c_ymin(SEXP geom);
extern SEXP geos_c_xmax(SEXP geom);
extern SEXP geos_c_ymax(SEXP geom);
extern SEXP geos_c_minimum_clearance(SEXP geom);
extern SEXP geos_c_is_empty(SEXP geom);
extern SEXP geos_c_is_simple(SEXP geom);
extern SEXP geos_c_is_ring(SEXP geom);
extern SEXP geos_c_has_z(SEXP geom);
extern SEXP geos_c_is_closed(SEXP geom);
extern SEXP geos_c_type_id(SEXP geom);
extern SEXP geos_c_precision(SEXP geom);
extern SEXP geos_c_srid(SEXP geom);
extern SEXP geos_c_num_coordinates(SEXP geom);
extern SEXP geos_c_num_geometries(SEXP geom);
extern SEXP geos_c_num_interior_rings(SEXP geom);
extern SEXP geos_c_dimension(SEXP geom);
extern SEXP geos_c_coorinate_dimension(SEXP geom);
extern SEXP geos_c_is_clockwise(SEXP geom);
extern SEXP geos_c_is_valid(SEXP geom);
extern SEXP geos_c_is_valid_detail(SEXP geom, SEXP allowSelfTouchingRingFormingHole);
extern SEXP geos_c_centroid(SEXP geom);
extern SEXP geos_c_boundary(SEXP geom);
extern SEXP geos_c_minimum_width(SEXP geom);
extern SEXP geos_c_minimum_clearance_line(SEXP geom);
extern SEXP geos_c_minimum_rotated_rectagle(SEXP geom);
extern SEXP geos_c_unary_union(SEXP geom);
extern SEXP geos_c_coverage_union(SEXP geom);
extern SEXP geos_c_point_on_surface(SEXP geom);
extern SEXP geos_c_node(SEXP geom);
extern SEXP geos_c_make_valid(SEXP geom);
extern SEXP geos_c_unique_points(SEXP geom);
extern SEXP geos_c_reverse(SEXP geom);
extern SEXP geos_c_merge_lines(SEXP geom);
extern SEXP geos_c_build_area(SEXP geom);
extern SEXP geos_c_envelope(SEXP geom);
extern SEXP geos_c_convex_hull(SEXP geom);
extern SEXP geos_c_point_start(SEXP geom);
extern SEXP geos_c_point_end(SEXP geom);
extern SEXP geos_c_line_merge(SEXP geom);
extern SEXP geos_c_clone(SEXP geom);
extern SEXP geos_c_constrained_delaunay_triangulation(SEXP geom);
extern SEXP geos_c_line_merge_directed(SEXP geom);
extern SEXP geos_c_make_valid_with_params(SEXP geom, SEXP params_sexp);
extern SEXP geos_c_interpolate(SEXP geom, SEXP param);
extern SEXP geos_c_interpolate_normalized(SEXP geom, SEXP param);
extern SEXP geos_c_point_n(SEXP geom, SEXP param);
extern SEXP geos_c_simplify(SEXP geom, SEXP param);
extern SEXP geos_c_simplify_preserve_topology(SEXP geom, SEXP param);
extern SEXP geos_c_unary_union_prec(SEXP geom, SEXP param);
extern SEXP geos_c_maximum_inscribed_circle(SEXP geom, SEXP param);
extern SEXP geos_c_densify(SEXP geom, SEXP param);
extern SEXP geos_c_remove_repeated_points(SEXP geom, SEXP param);
extern SEXP geos_c_set_precision(SEXP geom, SEXP param, SEXP preserveTopology, SEXP keepCollapsed);
extern SEXP geos_c_set_srid(SEXP geom, SEXP srid);
extern SEXP geos_c_normalize(SEXP geom);
extern SEXP geos_c_minimum_bounding_circle(SEXP geom);
extern SEXP geos_c_clip_by_rect(SEXP geom, SEXP xmin, SEXP ymin, SEXP xmax, SEXP ymax);
extern SEXP geos_c_delaunay_triangulation(SEXP geom, SEXP tolerace, SEXP edges);
extern SEXP geos_c_voronoi_diagram(SEXP geom, SEXP env, SEXP tolerace, SEXP edges);
extern SEXP geos_c_buffer(SEXP geom, SEXP distance, SEXP params);
extern SEXP geos_c_offset_curve(SEXP geom, SEXP distance, SEXP params);
extern SEXP geos_c_geometry_n(SEXP geom, SEXP n);
extern SEXP geos_c_ring_n(SEXP geom, SEXP n);
extern SEXP geos_c_geos_writer_new();
extern SEXP geos_c_wk_read_geos_geometry(SEXP geom, SEXP handler_xptr);
extern SEXP geos_c_init();
extern SEXP geos_c_version_runtime();
extern SEXP geos_c_version_build();

static const R_CallMethodDef CallEntries[] = {
    {"geos_c_distance", (DL_FUNC) &geos_c_distance, 2},
  {"geos_c_distance_indexed", (DL_FUNC) &geos_c_distance_indexed, 2},
  {"geos_c_distance_hausdorff", (DL_FUNC) &geos_c_distance_hausdorff, 2},
  {"geos_c_distance_frechet", (DL_FUNC) &geos_c_distance_frechet, 2},
  {"geos_c_prepared_distance", (DL_FUNC) &geos_c_prepared_distance, 2},
  {"geos_c_distance_hausdorff_densify", (DL_FUNC) &geos_c_distance_hausdorff_densify, 3},
  {"geos_c_distance_frechet_densify", (DL_FUNC) &geos_c_distance_frechet_densify, 3},
  {"geos_c_project", (DL_FUNC) &geos_c_project, 2},
  {"geos_c_project_normalized", (DL_FUNC) &geos_c_project_normalized, 2},
  {"geos_c_disjoint", (DL_FUNC) &geos_c_disjoint, 2},
  {"geos_c_touches", (DL_FUNC) &geos_c_touches, 2},
  {"geos_c_intersects", (DL_FUNC) &geos_c_intersects, 2},
  {"geos_c_crosses", (DL_FUNC) &geos_c_crosses, 2},
  {"geos_c_within", (DL_FUNC) &geos_c_within, 2},
  {"geos_c_contains", (DL_FUNC) &geos_c_contains, 2},
  {"geos_c_overlaps", (DL_FUNC) &geos_c_overlaps, 2},
  {"geos_c_equals", (DL_FUNC) &geos_c_equals, 2},
  {"geos_c_covers", (DL_FUNC) &geos_c_covers, 2},
  {"geos_c_covered_by", (DL_FUNC) &geos_c_covered_by, 2},
  {"geos_c_equals_exact", (DL_FUNC) &geos_c_equals_exact, 3},
  {"geos_c_is_within_distance", (DL_FUNC) &geos_c_is_within_distance, 3},
  {"geos_c_prepared_disjoint", (DL_FUNC) &geos_c_prepared_disjoint, 2},
  {"geos_c_prepared_touches", (DL_FUNC) &geos_c_prepared_touches, 2},
  {"geos_c_prepared_intersects", (DL_FUNC) &geos_c_prepared_intersects, 2},
  {"geos_c_prepared_crosses", (DL_FUNC) &geos_c_prepared_crosses, 2},
  {"geos_c_prepared_within", (DL_FUNC) &geos_c_prepared_within, 2},
  {"geos_c_prepared_contains", (DL_FUNC) &geos_c_prepared_contains, 2},
  {"geos_c_prepared_contains_properly", (DL_FUNC) &geos_c_prepared_contains_properly, 2},
  {"geos_c_prepared_overlaps", (DL_FUNC) &geos_c_prepared_overlaps, 2},
  {"geos_c_prepared_covers", (DL_FUNC) &geos_c_prepared_covers, 2},
  {"geos_c_prepared_covered_by", (DL_FUNC) &geos_c_prepared_covered_by, 2},
  {"geos_c_prepared_is_within_distance", (DL_FUNC) &geos_c_prepared_is_within_distance, 3},
  {"geos_c_relate", (DL_FUNC) &geos_c_relate, 3},
  {"geos_c_relate_pattern_match", (DL_FUNC) &geos_c_relate_pattern_match, 2},
  {"geos_c_intersection", (DL_FUNC) &geos_c_intersection, 2},
  {"geos_c_difference", (DL_FUNC) &geos_c_difference, 2},
  {"geos_c_sym_difference", (DL_FUNC) &geos_c_sym_difference, 2},
  {"geos_c_union", (DL_FUNC) &geos_c_union, 2},
  {"geos_c_shared_paths", (DL_FUNC) &geos_c_shared_paths, 2},
  {"geos_c_snap", (DL_FUNC) &geos_c_snap, 3},
  {"geos_c_intersection_prec", (DL_FUNC) &geos_c_intersection_prec, 3},
  {"geos_c_difference_prec", (DL_FUNC) &geos_c_difference_prec, 3},
  {"geos_c_sym_difference_prec", (DL_FUNC) &geos_c_sym_difference_prec, 3},
  {"geos_c_union_prec", (DL_FUNC) &geos_c_union_prec, 3},
  {"geos_c_largest_empty_circle", (DL_FUNC) &geos_c_largest_empty_circle, 3},
  {"geos_c_clearance_line_between", (DL_FUNC) &geos_c_clearance_line_between, 3},
  {"geos_c_geos_geometry_is_null", (DL_FUNC) &geos_c_geos_geometry_is_null, 1},
  {"geos_c_geos_geometry_is_null_or_xptr", (DL_FUNC) &geos_c_geos_geometry_is_null_or_xptr, 1},
  {"geos_c_read_wkt", (DL_FUNC) &geos_c_read_wkt, 1},
  {"geos_c_write_wkt", (DL_FUNC) &geos_c_write_wkt, 4},
  {"geos_c_read_geojson", (DL_FUNC) &geos_c_read_geojson, 1},
  {"geos_c_write_geojson", (DL_FUNC) &geos_c_write_geojson, 2},
  {"geos_c_read_wkb", (DL_FUNC) &geos_c_read_wkb, 1},
  {"geos_c_write_wkb", (DL_FUNC) &geos_c_write_wkb, 5},
  {"geos_c_read_hex", (DL_FUNC) &geos_c_read_hex, 1},
  {"geos_c_write_hex", (DL_FUNC) &geos_c_write_hex, 5},
  {"geos_c_write_xy", (DL_FUNC) &geos_c_write_xy, 1},
  {"geos_c_make_point", (DL_FUNC) &geos_c_make_point, 3},
  {"geos_c_make_linestring", (DL_FUNC) &geos_c_make_linestring, 4},
  {"geos_c_make_polygon", (DL_FUNC) &geos_c_make_polygon, 4},
  {"geos_c_make_collection", (DL_FUNC) &geos_c_make_collection, 3},
  {"geos_c_create_rectangle", (DL_FUNC) &geos_c_create_rectangle, 4},
  {"geos_c_empty", (DL_FUNC) &geos_c_empty, 1},
  {"geos_c_polygonize", (DL_FUNC) &geos_c_polygonize, 1},
  {"geos_c_polygonize_valid", (DL_FUNC) &geos_c_polygonize_valid, 1},
  {"geos_c_polygonize_cut_edges", (DL_FUNC) &geos_c_polygonize_cut_edges, 1},
  {"geos_c_polygonize_full", (DL_FUNC) &geos_c_polygonize_full, 1},
  {"geos_c_segment_intersection", (DL_FUNC) &geos_c_segment_intersection, 8},
  {"geos_c_orientation_index", (DL_FUNC) &geos_c_orientation_index, 6},
  {"geos_c_strtree_create", (DL_FUNC) &geos_c_strtree_create, 2},
  {"geos_c_strtree_data", (DL_FUNC) &geos_c_strtree_data, 1},
  {"geos_c_strtree_query", (DL_FUNC) &geos_c_strtree_query, 2},
  {"geos_c_touches_matrix", (DL_FUNC) &geos_c_touches_matrix, 2},
  {"geos_c_intersects_matrix", (DL_FUNC) &geos_c_intersects_matrix, 2},
  {"geos_c_crosses_matrix", (DL_FUNC) &geos_c_crosses_matrix, 2},
  {"geos_c_within_matrix", (DL_FUNC) &geos_c_within_matrix, 2},
  {"geos_c_contains_matrix", (DL_FUNC) &geos_c_contains_matrix, 2},
  {"geos_c_contains_properly_matrix", (DL_FUNC) &geos_c_contains_properly_matrix, 2},
  {"geos_c_overlaps_matrix", (DL_FUNC) &geos_c_overlaps_matrix, 2},
  {"geos_c_covers_matrix", (DL_FUNC) &geos_c_covers_matrix, 2},
  {"geos_c_covered_by_matrix", (DL_FUNC) &geos_c_covered_by_matrix, 2},
  {"geos_c_equals_matrix", (DL_FUNC) &geos_c_equals_matrix, 2},
  {"geos_c_equals_exact_matrix", (DL_FUNC) &geos_c_equals_exact_matrix, 3},
  {"geos_c_predicate_any", (DL_FUNC) &geos_c_predicate_any, 1},
  {"geos_c_nearest_error", (DL_FUNC) &geos_c_nearest_error, 2},
  {"geos_c_nearest", (DL_FUNC) &geos_c_nearest, 2},
  {"geos_c_nearest_indexed", (DL_FUNC) &geos_c_nearest_indexed, 2},
  {"geos_c_nearest_hausdorff", (DL_FUNC) &geos_c_nearest_hausdorff, 2},
  {"geos_c_nearest_frechet", (DL_FUNC) &geos_c_nearest_frechet, 2},
  {"geos_c_nearest_frechet_densify", (DL_FUNC) &geos_c_nearest_frechet_densify, 3},
  {"geos_c_nearest_hausdorff_densify", (DL_FUNC) &geos_c_nearest_hausdorff_densify, 3},
  {"geos_c_area", (DL_FUNC) &geos_c_area, 1},
  {"geos_c_length", (DL_FUNC) &geos_c_length, 1},
  {"geos_c_x", (DL_FUNC) &geos_c_x, 1},
  {"geos_c_y", (DL_FUNC) &geos_c_y, 1},
  {"geos_c_z", (DL_FUNC) &geos_c_z, 1},
  {"geos_c_xmin", (DL_FUNC) &geos_c_xmin, 1},
  {"geos_c_ymin", (DL_FUNC) &geos_c_ymin, 1},
  {"geos_c_xmax", (DL_FUNC) &geos_c_xmax, 1},
  {"geos_c_ymax", (DL_FUNC) &geos_c_ymax, 1},
  {"geos_c_minimum_clearance", (DL_FUNC) &geos_c_minimum_clearance, 1},
  {"geos_c_is_empty", (DL_FUNC) &geos_c_is_empty, 1},
  {"geos_c_is_simple", (DL_FUNC) &geos_c_is_simple, 1},
  {"geos_c_is_ring", (DL_FUNC) &geos_c_is_ring, 1},
  {"geos_c_has_z", (DL_FUNC) &geos_c_has_z, 1},
  {"geos_c_is_closed", (DL_FUNC) &geos_c_is_closed, 1},
  {"geos_c_type_id", (DL_FUNC) &geos_c_type_id, 1},
  {"geos_c_precision", (DL_FUNC) &geos_c_precision, 1},
  {"geos_c_srid", (DL_FUNC) &geos_c_srid, 1},
  {"geos_c_num_coordinates", (DL_FUNC) &geos_c_num_coordinates, 1},
  {"geos_c_num_geometries", (DL_FUNC) &geos_c_num_geometries, 1},
  {"geos_c_num_interior_rings", (DL_FUNC) &geos_c_num_interior_rings, 1},
  {"geos_c_dimension", (DL_FUNC) &geos_c_dimension, 1},
  {"geos_c_coorinate_dimension", (DL_FUNC) &geos_c_coorinate_dimension, 1},
  {"geos_c_is_clockwise", (DL_FUNC) &geos_c_is_clockwise, 1},
  {"geos_c_is_valid", (DL_FUNC) &geos_c_is_valid, 1},
  {"geos_c_is_valid_detail", (DL_FUNC) &geos_c_is_valid_detail, 2},
  {"geos_c_centroid", (DL_FUNC) &geos_c_centroid, 1},
  {"geos_c_boundary", (DL_FUNC) &geos_c_boundary, 1},
  {"geos_c_minimum_width", (DL_FUNC) &geos_c_minimum_width, 1},
  {"geos_c_minimum_clearance_line", (DL_FUNC) &geos_c_minimum_clearance_line, 1},
  {"geos_c_minimum_rotated_rectagle", (DL_FUNC) &geos_c_minimum_rotated_rectagle, 1},
  {"geos_c_unary_union", (DL_FUNC) &geos_c_unary_union, 1},
  {"geos_c_coverage_union", (DL_FUNC) &geos_c_coverage_union, 1},
  {"geos_c_point_on_surface", (DL_FUNC) &geos_c_point_on_surface, 1},
  {"geos_c_node", (DL_FUNC) &geos_c_node, 1},
  {"geos_c_make_valid", (DL_FUNC) &geos_c_make_valid, 1},
  {"geos_c_unique_points", (DL_FUNC) &geos_c_unique_points, 1},
  {"geos_c_reverse", (DL_FUNC) &geos_c_reverse, 1},
  {"geos_c_merge_lines", (DL_FUNC) &geos_c_merge_lines, 1},
  {"geos_c_build_area", (DL_FUNC) &geos_c_build_area, 1},
  {"geos_c_envelope", (DL_FUNC) &geos_c_envelope, 1},
  {"geos_c_convex_hull", (DL_FUNC) &geos_c_convex_hull, 1},
  {"geos_c_point_start", (DL_FUNC) &geos_c_point_start, 1},
  {"geos_c_point_end", (DL_FUNC) &geos_c_point_end, 1},
  {"geos_c_line_merge", (DL_FUNC) &geos_c_line_merge, 1},
  {"geos_c_clone", (DL_FUNC) &geos_c_clone, 1},
  {"geos_c_constrained_delaunay_triangulation", (DL_FUNC) &geos_c_constrained_delaunay_triangulation, 1},
  {"geos_c_line_merge_directed", (DL_FUNC) &geos_c_line_merge_directed, 1},
  {"geos_c_make_valid_with_params", (DL_FUNC) &geos_c_make_valid_with_params, 2},
  {"geos_c_interpolate", (DL_FUNC) &geos_c_interpolate, 2},
  {"geos_c_interpolate_normalized", (DL_FUNC) &geos_c_interpolate_normalized, 2},
  {"geos_c_point_n", (DL_FUNC) &geos_c_point_n, 2},
  {"geos_c_simplify", (DL_FUNC) &geos_c_simplify, 2},
  {"geos_c_simplify_preserve_topology", (DL_FUNC) &geos_c_simplify_preserve_topology, 2},
  {"geos_c_unary_union_prec", (DL_FUNC) &geos_c_unary_union_prec, 2},
  {"geos_c_maximum_inscribed_circle", (DL_FUNC) &geos_c_maximum_inscribed_circle, 2},
  {"geos_c_densify", (DL_FUNC) &geos_c_densify, 2},
  {"geos_c_remove_repeated_points", (DL_FUNC) &geos_c_remove_repeated_points, 2},
  {"geos_c_set_precision", (DL_FUNC) &geos_c_set_precision, 4},
  {"geos_c_set_srid", (DL_FUNC) &geos_c_set_srid, 2},
  {"geos_c_normalize", (DL_FUNC) &geos_c_normalize, 1},
  {"geos_c_minimum_bounding_circle", (DL_FUNC) &geos_c_minimum_bounding_circle, 1},
  {"geos_c_clip_by_rect", (DL_FUNC) &geos_c_clip_by_rect, 5},
  {"geos_c_delaunay_triangulation", (DL_FUNC) &geos_c_delaunay_triangulation, 3},
  {"geos_c_voronoi_diagram", (DL_FUNC) &geos_c_voronoi_diagram, 4},
  {"geos_c_buffer", (DL_FUNC) &geos_c_buffer, 3},
  {"geos_c_offset_curve", (DL_FUNC) &geos_c_offset_curve, 3},
  {"geos_c_geometry_n", (DL_FUNC) &geos_c_geometry_n, 2},
  {"geos_c_ring_n", (DL_FUNC) &geos_c_ring_n, 2},
  {"geos_c_geos_writer_new", (DL_FUNC) &geos_c_geos_writer_new, 0},
  {"geos_c_wk_read_geos_geometry", (DL_FUNC) &geos_c_wk_read_geos_geometry, 2},
  {"geos_c_init", (DL_FUNC) &geos_c_init, 0},
  {"geos_c_version_runtime", (DL_FUNC) &geos_c_version_runtime, 0},
  {"geos_c_version_build", (DL_FUNC) &geos_c_version_build, 0},
  {NULL, NULL, 0}
};
/* end generated by data-raw/make_callentries.R */

void R_init_geos(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

// # nocov start
void R_unload_geos(DllInfo *dll) {
  if (globalHandle != NULL) {
    GEOS_finish_r(globalHandle);
    globalHandle = NULL;
  }
}
// # nocov end

SEXP geos_c_init() {
  // load functions into the (currently NULL) function pointers in libgeos-impl.c
  libgeos_init_api();

  // create the global handle
  if (globalHandle == NULL) {
    globalHandle = GEOS_init_r();
    GEOSContext_setErrorMessageHandler_r(globalHandle, &geos_common_handle_error, globalErrorMessage);
    memset(globalErrorMessage, 0, sizeof(globalErrorMessage));
  }

  return R_NilValue;
}

SEXP geos_c_version_runtime() {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(GEOSversion()));
  UNPROTECT(1);
  return out;
}

SEXP geos_c_version_build() {
  SEXP out = PROTECT(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(GEOS_CAPI_VERSION));
  UNPROTECT(1);
  return out;
}
