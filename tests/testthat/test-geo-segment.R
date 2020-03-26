
test_that("geo_segment class works", {
  segment <- geo_segment(start = geo_xy(0, 0), end = geo_xy(1, 1))
  expect_output(print(segment), "geo_segment")
  expect_is(segment, "geo_segment")
  expect_true(is_geo_segment(segment))
  expect_true(vec_is(segment))
})


test_that("geo_segment c() works", {
  segment <- geo_segment(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6))
  tbl <- tibble(segment)
  df <- as.data.frame(tbl)
  tbl_segment <- as_tibble(segment)
  df_segment <- as.data.frame(tbl_segment)

  expect_is(c(segment, segment), "geo_segment")
  expect_length(c(segment, segment), 12)
  expect_is(vec_c(segment, segment), "geo_segment")
  expect_length(vec_c(segment, segment), 12)
  expect_equal(nrow(vec_rbind(tbl, tbl)), 12)
  expect_is(vec_rbind(tibble(segment), tibble(segment))$segment, "geo_segment")

  # check vec_c() with tibble and data frame types
  expect_identical(c(segment, tbl_segment), vec_rbind(tbl_segment, tbl_segment))
  expect_identical(vec_c(segment, tbl_segment), vec_rbind(tbl_segment, tbl_segment))
  # because there's no vec_ptype2.tbl_df generic anywhere, this returns a df
  expect_identical(vec_c(tbl_segment, segment), vec_rbind(df_segment, df_segment))
  expect_identical(vec_c(segment, df_segment), vec_rbind(df_segment, df_segment))
  expect_identical(vec_c(df_segment, segment), vec_rbind(df_segment, df_segment))
})

test_that("geo_segment casting works", {
  segment <- geo_segment(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6))

  expect_identical(tibble::as_tibble(segment), tibble(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6)))

  expect_identical(vec_cast(segment, geo_segment()), segment)
  expect_identical(vec_cast(segment, list()), vec_data(segment))
  expect_identical(vec_cast(vec_data(segment), geo_segment()), segment)

  expect_identical(
    vec_cast(tibble(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6)), geo_segment()),
    segment
  )

  expect_identical(
    as_geo_segment(tibble(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6))),
    segment
  )

  expect_identical(
    vec_c(
      geo_segment(start = geo_xy(0:1, 0:1), end = geo_xy(1:2, 1:2)),
      tibble(start = geo_xy(2:5, 2:5), end = geo_xy(3:6, 3:6))
    ),
    tibble::as_tibble(vec_data(segment))
  )
})
