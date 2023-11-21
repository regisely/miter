
check_col_name <- function(x, col) {
  has_col <- col %in% names(x)
  return(has_col)
}

check_types <- function(x, type) {
  has_type <- type %in% purrr::map(x, class)
  return(has_type)
}

check_col_type <- function(x, col, type) {
  has_col_type <- type %in% class(purrr::pluck(x, col, 1))
  return(has_col_type)
}

check_len_data <- function(x) {
  len_data <- nrow(purrr::pluck(x, "data", 1))
  if (len_data < 2) {
    rlang::abort("Less than two data points per group.")
  }
  invisible(len_data)
}

check_workflows <- function(workflows) {
  if (!is.list(workflows)) {
    rlang::abort("Argument `workflows` should be a list of workflows.")
  }

  if (!all(purrr::map_lgl(workflows, ~ inherits(.x, "workflow")))) {
    rlang::abort("All elements of `workflows` should have class workflow.")
  }

  wflow_names <- names(workflows)

  if (is.null(wflow_names)) {
    wflow_names <- paste0("model_", 1:length(workflows))
  }

  invisible(wflow_names)
}

check_column_arg <- function(x, arg) {
  arg_nm <- rlang::quo_name(arg)
  if (arg_nm == "splits") {
    has_splits <- check_col_name(x, "splits")
    if (!has_splits) {
      rlang::warn("No splits found. Resamples created on full data.")
      arg <- rlang::set_expr(arg, rlang::expr(data))
    }
  }

  arg
}

check_arg_len <- function(x, arg) {
  cl <- match.call()
  arg_list <- if (is.function(arg) | is.data.frame(arg)) list(arg) else arg
  n_arg <- length(arg_list)
  ids <- attr(x, "ids")
  n_ids <- x %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
    nrow()
  if (n_arg > n_ids) {
    rlang::warn(glue::glue("Length of `{cl$arg}` larger than number of ids. Some values will be ignored."))
  }

  invisible(arg_list)
}

check_fit_arg <- function(data, col) {
  ## allowed_cols <- c(
  ##   "data", "splits", "resamples_train", "resamples_data",
  ##   "nested_cv_train", "nested_cv_full"
  ## )
  ## if (!col %in% allowed_cols) {
  ##   rlang::abort("Column not allowed. Choose one of: `data`, `splits`, `resamples_train`, `resamples_full`, `nested_cv_train` or `nested_cv_full`")
  ## }
  has_col <- col %in% names(data)
  if (!has_col) {
    rlang::abort(glue::glue("Column `{col}` not found."))
  }

  col_resamples <- gsub("splits", "train", col)
  resamples_nm <- glue::glue("resamples_{col_resamples}")
  has_resamples <- resamples_nm %in% names(data)
  col_class <- class(purrr::pluck(data, col, 1))
  if (!"rset" %in% col_class) {
    has_tune <- any(
      purrr::map_lgl(data$workflows, ~ nrow(tune::tune_args(.x)) > 0)
    )
    if (has_tune & !has_resamples) {
      rlang::warn(glue::glue("No resamples found for `{col}`. Models that require tuning may fail. Run `cv_split()` or `cv_time_split()` before fitting on {col}. "))
    }
  }

  invisible(resamples_nm)
}

check_predict_arg <- function(data, col) {
  has_col <- col %in% names(data)
  if (!has_col) {
    rlang::abort(glue::glue("Column `{col}` not found."))
  }

  data_var <- gsub("fitted_", "", gsub("fitted_train", "splits", col))

  invisible(data_var)

}

check_metrics_arg <- function(data, col) {
  col_classes <- class(purrr::pluck(data, col, 1))
  is_resamples <- "resample_results" %in% col_classes
  is_tune <- "tune_results" %in% col_classes
  if (!(is_resamples | is_tune)) {
    rlang::abort("Argument `column` should contain resamples results.")
  }
  invisible(col_classes)
}

check_ts_models_packages <- function() {

  if (!requireNamespace("modeltime", quietly = TRUE)) {
    stop(
      "Package \"modeltime\" must be installed to use this function.",
      call. = FALSE
    )
  }

  invisible(NULL)

}

check_predictions <- function(data) {
  is_pred <- inherits(data, "miter_pred")
  if (!is_pred) {
    rlang::abort("Argument `data` should have class `miter_pred`.")
  }
  outcome_var <- attr(data, "outcome")
  if (!outcome_var %in% colnames(data)) {
    rlang::abort("True values of outcome variable should be present in predictions in order to create ensemble.")
  }
  if (nrow(data) < 1) {
    rlang::abort("Argument `data` has length zero.")
  }
  invisible(is_pred)
}

check_models_packages <- function() {

  if (!requireNamespace("modeltime", quietly = TRUE)) {
    stop(
      "Package \"modeltime\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop(
      "Package \"glmnet\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("kernlab", quietly = TRUE)) {
    stop(
      "Package \"kernlab\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop(
      "Package \"randomForest\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("earth", quietly = TRUE)) {
    stop(
      "Package \"earth\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop(
      "Package \"xgboost\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop(
      "Package \"ranger\" must be installed to use this function.",
      call. = FALSE
    )
  }

  invisible(NULL)

}
