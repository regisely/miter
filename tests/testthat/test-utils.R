test_that("generate_lags creates lagged variables", {
  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(state == "SP") %>%
    head(50)

  lagged <- generate_lags(icms_sample, icms, lags = 1)

  expect_true("icms_lag1" %in% names(lagged))
  expect_equal(nrow(lagged), nrow(icms_sample))
})

test_that("generate_lags works with multiple lags", {
  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(state == "SP") %>%
    head(50)

  lagged <- generate_lags(icms_sample, icms, lags = 1:3)

  expect_true(all(c("icms_lag1", "icms_lag2", "icms_lag3") %in% names(lagged)))
})
