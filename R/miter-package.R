#' @keywords internal
#' @importFrom dplyr across all_of any_of
#' @importFrom tidyr drop_na
#' @importFrom tidyselect where
#' @importFrom recipes step_rm
#' @importFrom tune collect_metrics
#' @importFrom stats as.formula sd
#' @importFrom grDevices hcl
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' miter: Iterative Modeling and Forecasting Tools
#'
#' @description
#' The miter package provides tools for iterative modeling and forecasting with
#' tidymodels. It facilitates creating nested tables for grouped time series data,
#' adding multiple modeling workflows, performing cross-validation, fitting models,
#' generating predictions, and creating ensemble models.
#'
#' @section Main Functions:
#'
#' **Creating Miter Tables:**
#' * `miter_table()`: Create a nested table with class `miter_tbl`
#' * `add_workflows()`: Add modeling workflows to a miter table
#'
#' **Data Splitting:**
#' * `holdout_split()`, `holdout_time_split()`: Create holdout splits
#' * `cv_split()`, `cv_time_split()`: Create cross-validation resamples
#' * `nested_cv_time_split()`: Create nested cross-validation for time series
#'
#' **Modeling:**
#' * `fit()`: Fit models to data in miter tables
#' * `predict()`: Generate predictions from fitted models
#' * `calculate_metrics()`: Calculate performance metrics
#'
#' **Model Initialization:**
#' * `initialize_ts_models()`: Initialize time series models
#' * `initialize_all_models()`: Initialize comprehensive model set
#' * `initialize_ensemble_models()`: Initialize ensemble models
#'
#' **Ensembles:**
#' * `create_ensemble()`: Create ensemble models from predictions
#' * `add_ensemble()`: Add ensemble predictions to existing predictions
#'
#' **Utilities:**
#' * `control_miter()`, `control_miter_race()`: Control parameters for fitting
#' * `autoplot()`: Plot predictions and metrics
#'
#' @section Typical Workflow:
#'
#' 1. Create a miter table from grouped data using `miter_table()`
#' 2. Add modeling workflows with `add_workflows()`
#' 3. Create train/test splits with `holdout_split()` or similar
#' 4. Optionally create resamples for tuning with `cv_split()` or similar
#' 5. Fit models with `fit()`
#' 6. Generate predictions with `predict()`
#' 7. Evaluate with `calculate_metrics()` and visualize with `autoplot()`
#' 8. Optionally create ensembles with `create_ensemble()`
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(miter)
#'
#' data(icms_br)
#'
#' # Initialize workflows
#' workflows <- initialize_ts_models(icms_br, "value", "uf", "date")
#'
#' # Create miter table and fit models
#' results <- icms_br %>%
#'   group_by(uf) %>%
#'   miter_table() %>%
#'   add_workflows(workflows) %>%
#'   holdout_time_split(prop = 0.8) %>%
#'   fit(splits)
#'
#' # Generate predictions
#' preds <- results %>%
#'   predict(fitted_splits)
#'
#' # Calculate metrics
#' metrics <- calculate_metrics(preds)
#'
#' # Visualize
#' autoplot(preds)
#' autoplot(metrics)
#' }
#'
#' @name miter-package
NULL
