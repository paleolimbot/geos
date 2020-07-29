
#' Unnest nested geometries
#'
#' @inheritParams geos_read_wkt
#' @inheritParams wkutils::wkt_unnest
#'
#' @return A [GEOS geometry vector][as_geos_geometry] with a length
#'   greater than or equal to `geom` with an attribute "lengths"
#'   that can be used to map elements of the result to the
#'   original item.
#' @export
#'
#' @examples
#' geos_unnest("GEOMETRYCOLLECTION (POINT (1 2), POINT (3 4))")
#'
geos_unnest <- function(geom, keep_empty = FALSE, keep_multi = TRUE, max_depth = 1) {
  # extract previous length information (if recursive call)
  lengths <- attr(geom, "lengths") %||% rep(1L, length(geom))

  geom <- as_geos_geometry(geom)

  # recursive base case
  if (max_depth == 0) {
    attr(geom, "lengths") <- lengths
    return(geom)
  }

  type_id <- geos_type_id(geom)
  is_collection <- type_id >= 4
  is_multi <- is_collection & type_id != 7
  is_empty <- geos_is_empty(geom)

  should_unnest <- !is.na(geom) & is_collection
  if (keep_empty) {
    should_unnest <- should_unnest & !is_empty
  }
  if (keep_multi) {
    should_unnest <- should_unnest & !is_multi
  }

  # early return skips the recursive call below
  if (!any(should_unnest)) {
    attr(geom, "lengths") <- lengths
    return(geom)
  }

  # do the actual unnesting
  to_unnest <- geom[should_unnest]
  unnest_lengths <- geos_num_geometries(to_unnest)
  unnested <- Map(geos_geometry_n, to_unnest, lapply(unnest_lengths, seq_len))

  # combine with the non-unnested items and unlist
  geom_list <- lapply(geom, list)
  geom_list[should_unnest] <- unnested
  result <- new_geos_geometry(unlist(geom_list))

  # do some bookkeeping to ensure unnested items can be mapped back to the
  # original item (possibly recursively)
  lengths <- attr(geom, "lengths") %||% rep(1L, length(geom))
  indices <- lengths_to_indices(lengths)
  new_lengths <- rep(1L, length(geom))
  new_lengths[should_unnest] <- unnest_lengths
  new_indices <- lengths_to_indices(new_lengths, indices = indices)
  final_lengths <- rle(new_indices)$lengths
  attr(result, "lengths") <- final_lengths

  # do more unnesting if max_depth allows
  geos_unnest(result, keep_empty = keep_empty, keep_multi = keep_multi, max_depth = max_depth - 1)
}

lengths_to_indices <- function(lengths, indices = seq_along(lengths)) {
  rle <- structure(
    list(lengths = lengths, values = indices),
    class = "rle"
  )

  inverse.rle(rle)
}
