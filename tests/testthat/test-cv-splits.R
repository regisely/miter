test_that("cv time split", {
  miter_tbl <- icms_br %>%
    holdout_time_split() %>%
    cv_time_split()
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("resamples_train" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 1)
})

test_that("cv time split full data", {
  miter_tbl <- icms_br %>%
    cv_time_split(data)
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("resamples_data" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 1)
})

test_that("cv time split no splits", {
  expect_warning(
    cv_time_split(icms_br),
    "No splits found. Resamples created on full data."
  )
})

test_that("cv time split with groups", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    holdout_time_split() %>%
    cv_time_split()
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("resamples_train" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 6)
})

test_that("cv time split with miter_tbl", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    miter_table() %>%
    cv_time_split(data)
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("resamples_data" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 6)
})

test_that("cv time split more args", {
  expect_warning(
    cv_time_split(icms_br, data, initial = c(80, 100)),
    "Length of `initial` larger than number of ids. Some values will be ignored."
  )
})

test_that("cv time split functions args", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    cv_time_split(
      data,
      initial = c(
        function(x) floor(nrow(x) * 3 / 4),
        function(x) floor(nrow(x) * 2 / 3),
        function(x) floor(nrow(x) * 1 / 3)
      )
    )
  expect_equal(nrow(miter_tbl$resamples_data[[1]]), 4)
  expect_equal(nrow(miter_tbl$resamples_data[[2]]), 5)
  expect_equal(nrow(miter_tbl$resamples_data[[3]]), 10)
})

test_that("cv time split slice_limits", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    cv_time_split(
      data,
      initial = 12,
      assess = 1,
      skip = 1,
      slice_limit = c(6, 12)
    )
  train <- rsample::analysis(miter_tbl$resamples_data[[1]]$splits[[1]])
  expect_equal(nrow(miter_tbl$resamples_data[[1]]), 6)
  expect_equal(nrow(miter_tbl$resamples_data[[2]]), 12)
  expect_equal(nrow(train), nrow(miter_tbl$data[[1]]) - 1)
})
