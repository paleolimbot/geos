
library(tidyverse)

pkg <- list.files("src", "\\.(h|cpp)$", full.names = TRUE) %>%
  lapply(read_file) %>%
  unlist() %>%
  str_extract_all("[0-9A-Za-z_]+_r\\(") %>%
  unlist() %>%
  str_remove("\\($") %>%
  unique()

h <- read_lines("data-raw/geos_c.h")
func_sum <- h %>%
  str_extract("GEOS_DLL [0-9A-Za-z_]+_r") %>%
  tibble(funcs = ., line = seq_along(.)) %>%
  filter(!is.na(funcs)) %>%
  separate(funcs, c("dll", "fun"), " ") %>%
  select(-dll) %>%
  mutate(
    used = fun %in% pkg,
    bullet = if_else(used, "[x]", "[ ]"),
    item = glue::glue("- {bullet} [{fun}](https://github.com/libgeos/geos/blob/master/capi/geos_c.h.in#L{line})")
  )

func_sum %>%
  pull(item) %>%
  str_c(collapse = "\n") %>%
  clipr::write_clip()
