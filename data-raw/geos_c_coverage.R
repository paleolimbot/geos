
library(tidyverse)

# download geoc_c.h for 3.8.1
curl::curl_download(
  "https://raw.githubusercontent.com/libgeos/geos/3.8.1/capi/geos_c.h.in",
  "data-raw/geos_c.h.in"
)

pkg <- list.files("src", "\\.(h|c)$", full.names = TRUE) %>%
  lapply(read_file) %>%
  unlist() %>%
  str_extract_all("[0-9A-Za-z_]+_r") %>%
  unlist() %>%
  unique()


ignore <- c(
  # not used for handling
  "GEOSContext_setErrorHandler_r",
  "GEOSContext_setNoticeHandler_r",

  # not needed
  "GEOSWKTWriter_getOutputDimension_r",
  "GEOSWKBWriter_getOutputDimension_r",
  "GEOSWKBWriter_getByteOrder_r",
  "GEOSWKBWriter_getIncludeSRID_r",
  "GEOSGeomType_r",
  "GEOSGeomGetLength_r",

  # deprecated
  "initGEOS_r",
  "finishGEOS_r",
  "GEOSGeomFromWKT_r",
  "GEOSGeomToWKT_r",
  "GEOSWKTWriter_setOld3D_r",
  "GEOS_getWKBOutputDims_r",
  "GEOS_setWKBOutputDims_r",
  "GEOS_getWKBByteOrder_r",
  "GEOS_setWKBByteOrder_r",
  "GEOSGeomFromWKB_buf_r",
  "GEOSGeomToWKB_buf_r",
  "GEOSGeomFromHEX_buf_r",
  "GEOSGeomToHEX_buf_r",
  "GEOSUnionCascaded_r",

  # using other buffer interface
  "GEOSBuffer_r",
  "GEOSBufferWithStyle_r",
  "GEOSSingleSidedBuffer_r"
)


h <- read_lines("data-raw/geos_c.h.in")
func_sum <- h %>%
  str_extract("GEOS_DLL\\s*\\*?\\s*[0-9A-Za-z_]+_r\\(") %>%
  str_remove("\\($") %>%
  tibble(funcs = ., line = seq_along(.)) %>%
  filter(!is.na(funcs)) %>%
  separate(funcs, c("dll", "fun"), "\\s+\\*?\\s*") %>%
  select(-dll) %>%
  mutate(
    used = fun %in% pkg,
    ignored = fun %in% ignore,
    bullet = case_when(ignored ~ "(ignored)", used ~ "[x]", TRUE ~ "[ ]"),
    item = glue::glue("- {bullet} [{fun}](https://github.com/libgeos/geos/blob/3.8.1/capi/geos_c.h.in#L{line})")
  )

func_sum %>%
  pull(item) %>%
  str_c(collapse = "\n") %>%
  clipr::write_clip()
