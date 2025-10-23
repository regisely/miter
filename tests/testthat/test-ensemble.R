test_that("create_ensemble works with predictions", {
  skip_if_not_installed("modeltime")

  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(state == "SP") %>%
    dplyr::filter(date >= as.Date("2010-01-01"))

  workflows <- initialize_ts_models(icms_sample, "icms", NULL, "date")

  tbl <- icms_sample %>%
    miter_table() %>%
    add_workflows(workflows[1:3]) %>%
    holdout_time_split(prop = 0.8)

  fitted <- fit(tbl, splits, control = control_miter(progress = FALSE, verbose = FALSE))
  preds <- predict(fitted, fitted_splits)

  ensemble_wflows <- initialize_ensemble_models(preds, "date")
  ensemble <- create_ensemble(preds, ensemble_wflows[1])

  expect_s3_class(ensemble, "miter_ensemble")
  expect_s3_class(ensemble, "miter_tbl")
})

test_that("add_ensemble combines predictions", {
  skip_if_not_installed("modeltime")

  data(icms_br)
  icms_sample <- icms_br %>%
    dplyr::filter(state == "SP") %>%
    dplyr::filter(date >= as.Date("2010-01-01"))

  workflows <- initialize_ts_models(icms_sample, "icms", NULL, "date")

  tbl <- icms_sample %>%
    miter_table() %>%
    add_workflows(workflows[1:3]) %>%
    holdout_time_split(prop = 0.8)

  fitted <- fit(tbl, splits, control = control_miter(progress = FALSE, verbose = FALSE))
  preds <- predict(fitted, fitted_splits)

  n_base_models <- length(unique(preds$models))

  ensemble_wflows <- initialize_ensemble_models(preds, "date")
  ensemble <- create_ensemble(preds, ensemble_wflows[1])
  combined <- add_ensemble(ensemble, preds)

  expect_s3_class(combined, "miter_pred")
  expect_true(length(unique(combined$models)) > n_base_models)
})
