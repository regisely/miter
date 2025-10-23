test_that("calculate_metrics works on miter_pred", {
  skip_if_not_installed("modeltime")

  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(uf %in% c("SP", "RJ")) %>%
    dplyr::filter(date >= as.Date("2010-01-01"))

  workflows <- initialize_ts_models(icms_sample, "value", "uf", "date")

  tbl <- icms_sample %>%
    dplyr::group_by(uf) %>%
    miter_table() %>%
    add_workflows(workflows[1:2]) %>%
    holdout_time_split(prop = 0.8)

  # Fit and predict
  fitted <- fit(tbl, splits, control = control_miter(progress = FALSE, verbose = FALSE))
  preds <- predict(fitted, fitted_splits)

  metrics <- calculate_metrics(preds)

  expect_s3_class(metrics, "miter_metrics")
  expect_true("metric" %in% names(metrics))
  expect_true("models" %in% names(metrics))
})

test_that("select_best parameter works", {
  skip_if_not_installed("modeltime")

  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(uf == "SP") %>%
    dplyr::filter(date >= as.Date("2010-01-01"))

  workflows <- initialize_ts_models(icms_sample, "value", NULL, "date")

  tbl <- icms_sample %>%
    miter_table() %>%
    add_workflows(workflows[1:2]) %>%
    holdout_time_split(prop = 0.8)

  fitted <- fit(tbl, splits, control = control_miter(progress = FALSE, verbose = FALSE))
  preds <- predict(fitted, fitted_splits)

  metrics_all <- calculate_metrics(preds)
  metrics_best <- calculate_metrics(preds, select_best = TRUE)

  expect_true(nrow(metrics_best) <= nrow(metrics_all))
})
