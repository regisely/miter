#' Create a time series recipe
#'
#' @description
#' Creates a simple recipe for time series modeling with a date variable as the
#' primary predictor.
#'
#' @param data A data frame.
#' @param outcome_var Character string naming the outcome variable.
#' @param id_var Character string naming the ID variable (optional).
#' @param date_var Character string naming the date variable.
#'
#' @return A `recipe` object.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' rec <- recipe_ts(icms_br, "value", "uf", "date")
#' }
#'
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

#' Initialize time series models
#'
#' @description
#' Creates a list of workflow objects for common time series models including
#' NAIVE, SNAIVE, ETS, ARIMA, and PROPHET. Requires the modeltime package.
#'
#' @param data A data frame containing the time series data.
#' @param outcome_var Character string naming the outcome variable.
#' @param id_var Character string naming the ID variable (optional).
#' @param date_var Character string naming the date variable.
#'
#' @return A named list of workflow objects for time series models.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' workflows <- initialize_ts_models(icms_br, "value", "uf", "date")
#' names(workflows)
#' }
#'
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

#' Initialize comprehensive set of models
#'
#' @description
#' Creates a list of workflow objects for a comprehensive set of time series and
#' machine learning models including NAIVE, SNAIVE, ETS, ARIMA, PROPHET, boosted
#' variants, GLMNET, Random Forest, XGBoost, MARS, and SVM. Requires modeltime
#' and additional modeling packages.
#'
#' @param data A data frame containing the time series data.
#' @param outcome_var Character string naming the outcome variable.
#' @param id_var Character string naming the ID variable (optional).
#' @param date_var Character string naming the date variable.
#' @param outcome_lags Integer specifying number of lags of outcome to include
#'   as features. Default is 0.
#' @param fix_arima_specs Logical. If TRUE, fits ARIMA models first to determine
#'   optimal specifications for each series. Default is FALSE.
#'
#' @return A named list of workflow objects for multiple model types.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' workflows <- initialize_all_models(
#'   icms_br,
#'   outcome_var = "value",
#'   id_var = "uf",
#'   date_var = "date",
#'   outcome_lags = 3
#' )
#' }
#'
#' @export
initialize_all_models <- function(data,
                                  outcome_var,
                                  id_var = NULL,
                                  date_var = NULL,
                                  outcome_lags = 0,
                                  fix_arima_specs = FALSE) {

  check_models_packages()

  require("modeltime")

  mv_vars <- colnames(data)[which(!colnames(data) %in% c(id_var, outcome_var))]
  formula_ts <- as.formula(paste0(outcome_var, " ~ ", "`", date_var, "`"))
  formula_mv <- as.formula(
    paste(outcome_var, "~", paste0("`", mv_vars, "`", collapse = " + "))
  )

  if (outcome_lags != 0) {
    data <- data %>%
      generate_lags(outcome_var, lags = outcome_lags) %>%
      drop_na()
    mv_vars <- colnames(data)[which(!colnames(data) %in% c(id_var, outcome_var))]
    formula_ml <- as.formula(
      paste(outcome_var, "~", paste0("`", mv_vars, "`", collapse = " + "))
    )
  } else {
    formula_ml <- formula_mv
  }

  recipe_ts <- recipes::recipe(formula_ts, data = data)
  recipe_mv <- recipes::recipe(formula_mv, data = data)
  recipe_ml <- recipes::recipe(formula_ml, data = data) %>%
    recipes::step_date(date_var) %>%
    recipes::step_rm(
               dplyr::contains("dow"),
               dplyr::contains("year"),
               date_var
             ) %>%
    recipes::step_dummy(glue::glue("{date_var}_month"))
  recipe_norm <- recipes::recipe(formula_ml, data = data) %>%
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

  if (fix_arima_specs) {
    arima_tbl <- data %>%
      dplyr::group_by(across({{ id_var }})) %>%
      add_workflows(list(ARIMA = wflow_arima)) %>%
      fit(data)
    
    arima_specs <- purrr::map(
      arima_tbl$fitted_data,
      ~ .x %>%
        hardhat::extract_fit_engine() %>%
        .$models %>%
        .$model_1 %>%
        .$arma
    )

    wflow_arima <- purrr::map(
      arima_specs,
      ~ workflows::workflow() %>%
        workflows::add_recipe(recipe_ts) %>%
        workflows::add_model(
          modeltime::arima_reg(
            non_seasonal_ar = !!.x[1],
            non_seasonal_differences = !!.x[6],
            non_seasonal_ma          = !!.x[2],
            seasonal_ar              = !!.x[3],
            seasonal_differences     = !!.x[7],
            seasonal_ma              = !!.x[4],
            seasonal_period          = !!.x[5]
          ) %>%
          parsnip::set_engine("arima")
        )
    )

    wflow_arima_boosted <- purrr::map(
      arima_specs,
      ~ workflows::workflow() %>%
        workflows::add_recipe(recipe_mv) %>%
        workflows::add_model(
          modeltime::arima_boost(
            non_seasonal_ar = !!.x[1],
            non_seasonal_differences = !!.x[6],
            non_seasonal_ma          = !!.x[2],
            seasonal_ar              = !!.x[3],
            seasonal_differences     = !!.x[7],
            seasonal_ma              = !!.x[4],
            seasonal_period          = !!.x[5],
            tree_depth = tune(),
            learn_rate = tune(),
            loss_reduction = tune(), 
            min_n = tune(),
            trees = tune()
          ) %>%
          parsnip::set_engine("arima_xgboost", method = "ML")
        )
    )
  } 

  wflow_prophet <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ts) %>%
    workflows::add_model(
      modeltime::prophet_reg(
        "regression"
      ) %>%
      parsnip::set_engine("prophet")
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

  wflow_rf <- workflows::workflow() %>%
    workflows::add_recipe(recipe_ml) %>%
    workflows::add_model(
      parsnip::rand_forest(
        min_n = tune(),
        trees = tune()
      ) %>% 
      parsnip::set_engine("ranger") %>%
      parsnip::set_mode("regression")
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

  wflows <- list(
    NAIVE = wflow_naive,
    SNAIVE = wflow_snaive,
    ETS = wflow_ets,
    ARIMA = wflow_arima,
    PROPHET = wflow_prophet,
    ARIMA_BOOSTED = wflow_arima_boosted,
    PROPHET_BOOSTED = wflow_prophet_boosted,
    GLMNET = wflow_glmnet,
    RANDOM_FOREST = wflow_rf,
    XGBOOST = wflow_xgboost,
    MARS = wflow_mars,
    SVM = wflow_svm
  )

  wflows
}

#' Initialize ensemble models
#'
#' @description
#' Creates ensemble model workflows that combine predictions from multiple base
#' models. Creates both linear model and GLMNET ensembles.
#'
#' @param preds A `miter_pred` object containing predictions from base models.
#' @param date_var Character string naming the date variable.
#'
#' @return A named list of workflow objects for ensemble models.
#'
#' @examples
#' \dontrun{
#' # After fitting base models and generating predictions
#' ensemble_workflows <- initialize_ensemble_models(predictions, "date")
#' }
#'
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
