### Testar com overlapping slices
base <- timetk::walmart_sales_weekly %>%
  dplyr::select(
           Store, Dept, Date, Weekly_Sales, Temperature,
           Fuel_Price, CPI, Unemployment
         )

wflows <- suppressMessages(
  initialize_all_models(base,
                        outcome_var = "Weekly_Sales",
                        date_var = "Date",
                        id_var = c("Store", "Dept"))
)

miter_tbl <- base %>%
  dplyr::group_by(Store, Dept) %>%
  add_workflows(wflows) %>%
  holdout_time_split() %>%
  cv_time_split() %>%
  cv_time_split(data) %>%
  nested_cv_time_split() %>%
  nested_cv_time_split(data)

miter_tbl <- miter_tbl %>%
  fit(resamples_data) %>%
  fit(resamples_train) %>%
  fit(splits) %>%
  fit(data)

pp <- miter_tbl %>%
  predict(fitted_resamples_train, bind = "all") 

pp_final <- miter_tbl %>%
  predict(fitted_resamples_data, bind = "all") 

pp %>% autoplot()

pp_test <- miter_tbl %>%
  predict(fitted_train, bind = "all")

wflow_ensemble <- initialize_ensemble_models(pp_final)

ee <- create_ensemble(pp_final, wflow_ensemble, select_best = 5)

preds <- ee %>%
  add_ensemble(pp_final)

preds_test <- ee %>%
  add_ensemble(pp_test)

plotly::ggplotly(preds %>% autoplot())

plotly::ggplotly(preds_test %>% autoplot())

preds_test %>%
  calculate_metrics() %>%
  dplyr::arrange(Store, Dept, rank) %>%
  View
