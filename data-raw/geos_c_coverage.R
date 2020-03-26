
library(tidyverse)

pkg <- list.files("src", "\\.(h|cpp)$", full.names = TRUE) %>%
  lapply(read_file) %>%
  unlist() %>%
  str_extract_all("[0-9A-Za-z_]+_r\\(") %>%
  unlist() %>%
  str_remove("\\($") %>%
  unique()


ignore <- c(
  # deprecated
  "GEOSGeomFromWKT_r",
  "GEOSGeomToWKT_r",
  "GEOS_getWKBOutputDims_r",
  "GEOS_setWKBOutputDims_r",
  "GEOS_getWKBByteOrder_r",
  "GEOS_setWKBByteOrder_r",
  "GEOSGeomFromWKB_buf_r",
  "GEOSGeomToWKB_buf_r",
  "GEOSGeomFromHEX_buf_r",
  "GEOSGeomToHEX_buf_r",
  "GEOSUnionCascaded_r",

  # not in GEOS 3.8 (what I have)
  "GEOSCoordSeq_setXY_r",
  "GEOSCoordSeq_setXYZ_r",
  "GEOSCoordSeq_getXY_r",
  "GEOSCoordSeq_getXYZ_r",
  "GEOSGeom_createPointFromXY_r",

  # using other buffer interface
  "GEOSBuffer_r",
  "GEOSBufferWithStyle_r"
)


h <- read_lines("data-raw/geos_c.h")
func_sum <- h %>%
  str_extract("GEOS_DLL\\s*\\*?\\s*[0-9A-Za-z_]+_r") %>%
  tibble(funcs = ., line = seq_along(.)) %>%
  filter(!is.na(funcs)) %>%
  separate(funcs, c("dll", "fun"), "\\s+\\*?\\s*") %>%
  select(-dll) %>%
  mutate(
    used = fun %in% pkg,
    ignored = fun %in% ignore,
    bullet = case_when(ignored ~ "(ignored)", used ~ "[x]", TRUE ~ "[ ]"),
    item = glue::glue("- {bullet} [{fun}](https://github.com/libgeos/geos/blob/master/capi/geos_c.h.in#L{line})")
  )

func_sum %>%
  pull(item) %>%
  str_c(collapse = "\n") %>%
  clipr::write_clip()
