test_that("holdout_time_split", {
  miter_tbl <- icms_br %>%
    holdout_time_split()
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("splits" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 1)
})

test_that("holdout_time_split grouped", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    holdout_time_split()
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("splits" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 6)
})

test_that("holdout_time_split miter_tbl", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    as_miter_table() %>%
    holdout_time_split()
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("splits" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 6)
})

test_that("holdout_time_split already nested", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    tidyr::nest() %>%
    holdout_time_split()
  expect_s3_class(miter_tbl, "miter_tbl")
  expect_true("splits" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 6)
})


test_that("holdout_time_split prop argument larger than ids", {
  expect_warning(
    holdout_time_split(icms_br, prop = c(0.9, 0.8)),
    "Length of `prop` larger than number of ids. Some values will be ignored."
  )
})

test_that("holdout_time_split function in prop argument", {
  miter_tbl <- icms_br %>%
    dplyr::group_by(state) %>%
    
  expect_warning(
    holdout_time_split(icms_br, prop = c(0.9, 0.8)),
    "Length of `prop` larger than number of ids. Some values will be ignored."
  )
})
