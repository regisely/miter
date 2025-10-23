test_that("generate_lags creates lagged variables", {
  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(uf == "SP") %>%
    head(50)

  lagged <- generate_lags(icms_sample, value, lags = 1)

  expect_true("value_lag1" %in% names(lagged))
  expect_equal(nrow(lagged), nrow(icms_sample))
})

test_that("generate_lags works with multiple lags", {
  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(uf == "SP") %>%
    head(50)

  lagged <- generate_lags(icms_sample, value, lags = 1:3)

  expect_true(all(c("value_lag1", "value_lag2", "value_lag3") %in% names(lagged)))
})
