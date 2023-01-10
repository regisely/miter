test_that("miter_tbl class", {
  miter_tbl <- miter_table(icms_br)
  expect_s3_class(miter_tbl, "miter_tbl")
})

test_that("miter_tbl class already nested", {
  miter_tbl <- icms_br %>%
    tidyr::nest(data = everything()) %>%
    miter_table()
  expect_s3_class(miter_tbl, "miter_tbl")
})

test_that("data already nested without data column", {
  miter_tbl <- icms_br %>%
    tidyr::nest(other = icms)
  expect_error(
    miter_table(miter_tbl),
    "List-columns not allowed before creating miter table."
  )
})

test_that("miter_tbl class already nested with groups", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    tidyr::nest() %>%
    miter_table()
  expect_true(!inherits(miter_tbl, "grouped_df"))
  expect_s3_class(miter_tbl, "miter_tbl")
})

test_that("grouped data already without data column", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    tidyr::nest(other = icms)
  expect_error(
    miter_table(miter_tbl),
    "List-columns not allowed before creating miter table."
  )
})

test_that("less than two points per group", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    tidyr::nest(data = icms)
  expect_error(
    miter_table(miter_tbl),
    "Less than two data points per group."
  )
})

test_that("data column in miter_tbl", {
  miter_tbl <- icms_br %>%
    miter_table()
  expect_true("data" %in% names(miter_tbl))
})

test_that("rows miter_tbl with no grouping", {
  miter_tbl <- icms_br %>%
    miter_table()
  expect_equal(nrow(miter_tbl), 1)
})

test_that("rows miter_tbl with grouping", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    miter_table()
  expect_equal(nrow(miter_tbl), 6)
})
