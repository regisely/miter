### Testar com overlapping slices
wflows <- suppressMessages(
  initialize_all_models(icms_br,
                        outcome_var = "icms",
                        date_var = "date",
                        id_var = "state")
)

wflows <- wflows[-c(1, 2, 6, 7, 8)]

miter_tbl <- icms_br %>%
  dplyr::rename(id = state) %>%
  dplyr::group_by(id) %>%
  add_workflows(wflows) %>%
  holdout_time_split() %>%
  cv_time_split() %>%
  cv_time_split(data) %>%
  nested_cv_time_split() %>%
  nested_cv_time_split(data)

library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makeCluster(all_cores)
doParallel::registerDoParallel(cl)

miter_tbl <- miter_tbl %>%
  fit(resamples_data) %>%
  fit(resamples_train) %>%
  fit(splits) %>%
  fit(data) %>%
  fit(nested_cv_data)

miter_tbl %>% predict(fitted_nested_cv_data)

preds_outer <- miter_tbl %>%
  predict(fitted_nested_cv_data, bind = "predictors")

preds_inner <- miter_tbl %>%
  predict(fitted_nested_cv_data, inner = TRUE, bind = "predictors")

wflow_ensemble <- initialize_ensemble_models(
  preds_inner %>%
  dplyr::filter(.id_resamples == .id_resamples[1L])
)

ee <- purrr::map2_dfr(
  preds_inner %>%
  dplyr::select(-.id_inner) %>%
  dplyr::group_split(.id_resamples),
  preds_outer %>%
  dplyr::group_split(.id_resamples, .keep = FALSE),
  function(x, y) {
    pp <- x %>%
      dplyr::select(-.id_resamples) %>%
      rsample:::add_class("miter_pred") %>%
      add_attr("outcome", "icms") %>%
      add_attr("ids", "id")
    pp_out <- y %>%
      rsample:::add_class("miter_pred") %>%
      add_attr("outcome", "icms") %>%
      add_attr("ids", "id")
    create_ensemble(pp, wflow_ensemble) %>%
      add_ensemble(pp_out) %>%
      dplyr::mutate(.id_resamples = purrr::pluck(x, ".id_resamples", 1)) %>%
      dplyr::relocate(.id_resamples, .after = models)
  })

plotly::ggplotly(ee %>% calculate_metrics(resamples = TRUE) %>% autoplot())

preds_outer %>%
  create_ensemble(wflow_ensemble) %>%
  add_ensemble(pp_test) %>%
  calculate_metrics() %>%
  autoplot()


pp <- miter_tbl %>%
  predict(fitted_resamples_train, bind = "predictors")

pp_test <- miter_tbl %>%
  predict(fitted_train, bind = "all")

wflow_ensemble <- initialize_ensemble_models(pp)

ee <- create_ensemble(pp, wflow_ensemble, n_models = 5, fit_resamples = TRUE)

ee %>% calculate_metrics(fitted_resamples_data)

miter_tbl %>%
  predict(fitted_resamples_train) %>%
  create_ensemble(wflow_ensemble) %>%
  add_ensemble(pp_test) %>%
  calculate_metrics() %>%
  autoplot()

preds %>% autoplot()

preds %>%
  calculate_metrics()

add_ensemble(ee, pp) %>% calculate_metrics(resamples = TRUE)
add_ensemble(ee, pp_test) %>% calculate_metrics()


add_ensemble(ee, pp) %>% autoplot()
add_ensemble(ee, pp_test) %>% autoplot()




dplyr::bind_rows(
  calculate_metrics(miter_tbl, fitted_resamples_data) %>%
  dplyr::select(-rank),
  ee_pred %>%
  dplyr::group_by(state, id) %>%
  rmse(icms, .pred) %>%
  dplyr::group_by(state) %>%
  dplyr::summarise(metric = mean(.estimate), std_err = sd(.estimate)/sqrt(5),
                   .groups = "drop") %>%
  dplyr::mutate(models = "ENSEMBLE")
  ) %>%
  dplyr::arrange(state, metric)



mm <- miter_tbl %>%
  calculate_metrics(fitted_resamples_train)

ee_test <- create_ensemble(pp_test , wflow_ensemble)

ee_final <- ee %>%
  predict(fitted_data, new_data = ee_test$data, bind = "new_data")

calculate_metrics(pp)
calculate_metrics(ee_pred)

calculate_metrics(pp_test)
calculate_metrics(ee_final)

pp
calculate_metrics(miter_tbl, fitted_resamples_data)



calculate_metrics(ee, fitted_resamples_data)

calculate_metrics(ee_pred)

miter_tbl %>%
  predict(
    fitted_train, bind = "outcome",
    new_data = list(
      miter_tbl$data[[1]][140:144,], miter_tbl$data[[1]][130:144,]
    )
  )

icms_fut <- icms_br %>%
  dplyr::filter(state == "Bahia") %>%
  timetk::future_frame(.date_var = date, .length_out = 12) %>%
  dplyr::mutate(icms = NA)

icms_pred <- miter_tbl %>%
  predict(fitted_data, new_data = icms_fut, bind = "all")

icms_pred <- miter_tbl %>%
  predict(fitted_train, bind = "all") %>%
  mutate(.pred = ifelse(models %in% c("ETS", "ARIMA", "PROPHET"), exp(.pred), .pred))

icms_pred <- miter_tbl %>%
  predict(fitted_resamples_full, bind = "all") %>%
  mutate(.pred = ifelse(models %in% c("ETS", "ARIMA", "PROPHET"), exp(.pred), .pred))

autoplot(icms_pred)


test_that("add workflows", {
  miter_tbl <- icms_br %>%
    add_workflows(wflows)
  expect_true("workflows" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 4)
  expect_s3_class(miter_tbl, "miter_tbl")
})

test_that("add workflows with groups", {
  miter_tbl <- icms_br %>%
    group_by(state) %>%
    add_workflows(wflows)
  expect_true("workflows" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 24)
  expect_s3_class(miter_tbl, "miter_tbl")
})

test_that("add workflows with miter_tbl", {
  miter_tbl <- icms_br %>%
    group_by(state) %>%
    as_miter_table() %>%
    add_workflows(wflows)
  expect_true("workflows" %in% names(miter_tbl))
  expect_equal(nrow(miter_tbl), 24)
  expect_s3_class(miter_tbl, "miter_tbl")
})
