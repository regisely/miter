##### Consertar initialize_all_models quando date_var=NULL
#' @importFrom hardhat tune
#' @export
recipe_ts <- function(data,
                      outcome_var,
                      id_var = NULL,
                      date_var = NULL) {

  mv_vars <- colnames(data)[which(!colnames(data) %in%
                                  c(id_var, outcome_var))]
  formula_ts <- as.formula(
    paste0(outcome_var, " ~ ", "`", date_var, "`")
  )
  recipe_ts <- recipes::recipe(formula_ts, data = data)
  recipe_ts
  
}

#' @export
initialize_ts_models <- function(data,
                                 outcome_var,
                                 id_var = NULL,
                                 date_var = NULL) {

  check_ts_models_packages()

  require("modeltime")

  mv_vars <- colnames(data)[which(!colnames(data) %in%
                                  c(id_var, outcome_var))]
  formula_ts <- as.formula(
    paste0(outcome_var, " ~ ", "`", date_var, "`")
  )
  recipe_ts <- recipes::recipe(formula_ts, data = data)

  wflow_naive <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::naive_reg() %>%
      parsnip::set_engine("naive")
    )

  wflow_snaive <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::naive_reg() %>%
      parsnip::set_engine("snaive")
    )
  
  wflow_ets <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::exp_smoothing() %>%
      parsnip::set_engine("ets")
    )

  wflow_arima <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::arima_reg() %>%
      parsnip::set_engine("auto_arima")
    )

  wflow_prophet <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::prophet_reg(
        "regression"
      ) %>%
      parsnip::set_engine("prophet")
    )

  wflows <- list(
    NAIVE = wflow_naive,
    SNAIVE = wflow_snaive,
    ETS = wflow_ets,
    ARIMA = wflow_arima,
    PROPHET = wflow_prophet
  )

  wflows
  
}

#' @export
initialize_all_models <- function(data,
                                  outcome_var,
                                  id_var = NULL,
                                  date_var = NULL) {

  check_models_packages()

  require("modeltime")

  mv_vars <- colnames(data)[which(!colnames(data) %in% c(id_var, outcome_var))]
  formula_ts <- as.formula(paste0(outcome_var, " ~ ", "`", date_var, "`"))
  formula_mv <- as.formula(
    paste(outcome_var, "~", paste0("`", mv_vars, "`", collapse = " + "))
  )

  recipe_ts <- recipes::recipe(formula_ts, data = data)
  recipe_mv <- recipes::recipe(formula_mv, data = data)
  recipe_ml <- recipe_mv %>%
    recipes::step_date(date_var) %>%
    recipes::step_rm(
               dplyr::contains("dow"),
               dplyr::contains("year"),
               date_var
             ) %>%
    recipes::step_dummy(glue::glue("{date_var}_month"))
  recipe_norm <- recipe_mv %>%
    recipes::step_normalize(mv_vars[which(mv_vars != date_var)]) %>%
    recipes::step_date(date_var) %>%
    recipes::step_rm(
               dplyr::contains("dow"),
               dplyr::contains("year"),
               date_var
             ) %>%
    recipes::step_dummy(glue::glue("{date_var}_month"))

  wflow_naive <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::naive_reg() %>%
      parsnip::set_engine("naive")
    )

  wflow_snaive <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::naive_reg() %>%
      parsnip::set_engine("snaive")
    )
  
  wflow_ets <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::exp_smoothing() %>%
      parsnip::set_engine("ets")
    )

  wflow_arima <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::arima_reg() %>%
      parsnip::set_engine("auto_arima")
    )

  wflow_prophet <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::prophet_reg(
        "regression"
      ) %>%
      parsnip::set_engine("prophet")
    )

  wflow_arima_boosted <- workflows::workflow() %>%
    workflows::add_recipe(recipe_mv) %>%
    workflows::add_model(
      modeltime::arima_boost(
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(), 
        min_n = tune(),
        trees = tune()
        ) %>%
      parsnip::set_engine("auto_arima_xgboost")
    )

  wflow_prophet_boosted <-  workflows::workflow() %>%
    workflows::add_recipe(recipe_mv) %>%
    workflows::add_model(
      modeltime::prophet_boost(
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(), 
        min_n = tune(),
        trees = tune()
      ) %>% 
      parsnip::set_engine("prophet_xgboost")
    )

  wflow_glmnet <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ml) %>%
    workflows::add_model(
      parsnip::linear_reg(
        penalty = tune(),
        mixture = tune()
      ) %>%
      parsnip::set_engine("glmnet")
    )

  wflow_xgboost <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ml) %>%
    workflows::add_model(
      parsnip::boost_tree(
        trees = tune(),
        min_n = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(), 
        sample_size = tune()
      ) %>% 
      parsnip::set_engine("xgboost") %>%
      parsnip::set_mode("regression")
    )
  
  wflow_mars <-  workflows::workflow() %>%
    workflows::add_recipe(recipe_ml) %>%
    workflows::add_model(
      parsnip::mars(
        num_terms = tune(),
        prod_degree = tune()
      ) %>%
      parsnip::set_engine("earth") %>%
      parsnip::set_mode("regression")
    )
  
  wflow_svm <- workflows::workflow() %>%
    workflows::add_recipe(recipe_norm) %>%
    workflows::add_model(
      parsnip::svm_rbf(
        cost = tune(),
        rbf_sigma = tune(),
        margin = tune()
      ) %>% 
      parsnip::set_engine("kernlab") %>%
      parsnip::set_mode("regression")
    )

  wflow_rf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_norm) %>%
    workflows::add_model(
      parsnip::rand_forest(
        min_n = tune(),
        trees = tune()
      ) %>% 
      parsnip::set_engine("ranger") %>%
      parsnip::set_mode("regression")
    )

  wflows <- list(
    NAIVE = wflow_naive,
    SNAIVE = wflow_snaive,
    ETS = wflow_ets,
    ARIMA = wflow_arima,
    PROPHET = wflow_prophet,
    ARIMA_BOOSTED = wflow_arima_boosted,
    PROPHET_BOOSTED = wflow_prophet_boosted,
    GLMNET = wflow_glmnet,
    XGBOOST = wflow_xgboost,
    MARS = wflow_mars,
    SVM = wflow_svm,
    RANDOM_FOREST = wflow_rf
  )

  wflows
}

#' @export
initialize_ensemble_models <- function(preds, date_var) {

  ids <- attr(preds, "ids")
  outcome_var <- attr(preds, "outcome")

  ## MAKE A CHECK FOR RESAMPLES AND SELECT_BEST (is.numeric) AND bind outcome
  preds <- preds %>%
    tidyr::drop_na(.pred) %>%
    dplyr::select(
             dplyr::all_of(ids),
             models,
             dplyr::all_of(date_var),
             dplyr::all_of(outcome_var),
             .pred
           ) %>%
    tidyr::pivot_wider(names_from = "models", values_from = ".pred") %>%
    dplyr::select(-ids)

  formula_ensemble <- as.formula(
    paste(outcome_var, "~ .")
  )
  formula_lm <- as.formula(
    paste(outcome_var, "~ 0 + .")
  )
  recipe_ensemble <- recipes::recipe(formula_ensemble, data = preds) %>%
    step_rm(date_var)

  wflow_lm <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ensemble) %>%
    workflows::add_model(
      parsnip::linear_reg() %>%
      parsnip::set_engine("lm"),
      formula = formula_lm
    )

  wflow_glmnet <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ensemble) %>%
    workflows::add_model(
      parsnip::linear_reg(
        penalty = tune(),
        mixture = tune()
      ) %>%
      parsnip::set_engine("glmnet", intercept = FALSE)
    )

  wflows <- list(
    ENSEMBLE_LM = wflow_lm,
    ENSEMBLE_GLMNET = wflow_glmnet
  )

  wflows
}
