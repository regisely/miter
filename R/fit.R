#### No progress when using fit only
#' @importFrom generics fit
#' @importFrom yardstick metric_set rmse
#' @export
fit.miter_tbl <- function(object,
                          column,
                          metric = metric_set(rmse),
                          grid = 10,
                          control = control_miter(),
                          param_info = NULL, ...) {
  ids <- attr(object, "ids")
  classes <- attr(object, "class")
  log_env <- rlang::env()

  has_workflow <- check_col_name(object, "workflows")
  if (!has_workflow) {
    rlang::abort("Workflows not found. Provide a list of workflows by running `add_workflows` before fitting.")
  }

  arg <- rlang::enquo(column)
  arg_nm <- rlang::quo_name(arg)
  resamples_nm <- check_fit_arg(object, arg_nm)
  has_resamples <- resamples_nm %in% names(object)
  fitted_cv_nm <- glue::glue("fitted_{resamples_nm}")
  has_fitted_cv <- fitted_cv_nm %in% names(object)
  arg_adj <- gsub("splits", "train", arg_nm)

  if (control$progress) {
    cli::cli_progress_bar(
      glue::glue("Fitting models to {arg_nm}."),
      .envir = log_env, total = nrow(object)
    )
  }

  out <- object %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "fitted_{arg_adj}" := list(
        try(
          miter_fit(
            !!arg,
            workflows,
            fitted_cv = if (has_fitted_cv) .data[[fitted_cv_nm]] else NULL,
            resamples = if (has_resamples) .data[[resamples_nm]] else NULL,
            log_env = log_env,
            metric = metric,
            grid = grid,
            control = control,
            param_info = param_info,
            ...
          ),
          silent = TRUE
        )
      )
    ) %>%
    dplyr::ungroup()

  if (control$progress) cli::cli_progress_done(.envir = log_env)

  attr(out, "ids") <- ids
  class(out) <- classes

  out
}

#' @export
miter_fit <- function(object, workflow,
                      fitted_cv = NULL, resamples = NULL,
                      log_env = rlang::env(),
                      metric = metric_set(rmse),
                      grid = 10,
                      control = control_miter(),
                      param_info = NULL, ...) {
  UseMethod("miter_fit")
}

#' @export
miter_fit.tbl_df <- function(data, workflow,
                             fitted_cv = NULL, resamples = NULL,
                             log_env = rlang::env(),
                             metric = metric_set(rmse),
                             grid = 10,
                             control = control_miter(),
                             param_info = NULL, ...) {
  has_tune <- nrow(tune::tune_args(workflow)) > 0
  control_tune <- control_miter_to_tune(control)

  if (has_tune) {
    if (is.null(fitted_cv)) {
      if (control$verbose) {
        set.seed(123)
        fitted_cv <- tune::tune_grid(
          object = workflow,
          resamples = resamples,
          metrics = metric,
          grid = grid,
          control = control_tune,
          param_info = param_info
        )
      } else {
        set.seed(123)
        fitted_cv <- suppressMessages(
          tune::tune_grid(
            object = workflow,
            resamples = resamples,
            metrics = metric,
            grid = grid,
            control = control_tune,
            param_info = param_info
          )
        )
      }
    }
    best_parameters <- tune::select_best(fitted_cv)
    workflow <- tune::finalize_workflow(workflow, best_parameters)
  }

  if (control$verbose) {
    out <- fit(
      object = workflow,
      data = data,
      ...
    )
  } else {
    out <- suppressMessages(
      fit(
        object = workflow,
        data = data,
        ...
      )
    )
  }

  if (control$progress) {
    try(cli::cli_progress_update(.envir = log_env), silent = TRUE)
  }

  out
}

#' @export
miter_fit.rsplit <- function(data, workflow,
                             fitted_cv = NULL, resamples = NULL,
                             log_env = rlang::env(),
                             metric = metric_set(rmse),
                             grid = 10,
                             control = control_miter(),
                             param_info = NULL, ...) {

  has_tune <- nrow(tune::tune_args(workflow)) > 0
  control_tune <- control_miter_to_tune(control)

  if (has_tune) {
    if (is.null(fitted_cv)) {
      if (control$verbose) {
        set.seed(123)
        fitted_cv <- tune::tune_grid(
          object = workflow,
          resamples = resamples,
          metrics = metric,
          grid = grid,
          control = control_tune,
          param_info = param_info
        )
      } else {
        set.seed(123)
        fitted_cv <- suppressMessages(
          tune::tune_grid(
            object = workflow,
            resamples = resamples,
            metrics = metric,
            grid = grid,
            control = control_tune,
            param_info = param_info
          )
        )
      }
    }
    best_parameters <- tune::select_best(fitted_cv)
    workflow <- tune::finalize_workflow(workflow, best_parameters)
  }

  if (control$verbose) {
    out <- fit(
      object = workflow,
      data = rsample::training(data),
      ...
    )
  } else {
    out <- suppressMessages(
      fit(
        object = workflow,
        data = rsample::training(data),
        ...
      )
    )
  }

  if (control$progress) {
    try(cli::cli_progress_update(.envir = log_env), silent = TRUE)
  }

  out
}

#' @export
miter_fit.rset <- function(data, workflow,
                           fitted_cv = NULL, resamples = NULL,
                           log_env = rlang::env(),
                           metric = metric_set(rmse),
                           grid = 10,
                           control = control_miter(),
                           param_info = NULL, ...) {
  has_tune <- nrow(tune::tune_args(workflow)) > 0
  control_tune <- control_miter_to_tune(control)

  if (has_tune) {
    if (control$verbose) {
      set.seed(123)
      out <- tune::tune_grid(
        object = workflow,
        resamples = data,
        metrics = metric,
        grid = grid,
        control = control_tune,
        param_info = param_info
      )
    } else {
      set.seed(123)
      out <- suppressMessages(
        tune::tune_grid(
          object = workflow,
          resamples = data,
          metrics = metric,
          grid = grid,
          control = control_tune,
          param_info = param_info
        )
      )
    }
  } else {
    if (control$verbose) {
      set.seed(123)
      out <- tune::fit_resamples(
        object = workflow,
        resamples = data,
        metrics = metric,
        control = control_tune
      )
    } else {
      set.seed(123)
      out <- suppressMessages(
        tune::fit_resamples(
          object = workflow,
          resamples = data,
          metrics = metric,
          control = control_tune
        )
      )
    }
  }

  if (control$progress) {
    try(cli::cli_progress_update(.envir = log_env), silent = TRUE)
  }

  out
}

#' @export
print.nested_results <- function(x, ...) {
  cat("# Nested Resampling Results\n")
  class(x) <- class(x)[!(class(x) %in% c("nested_results"))]
  print(x, ...)
}

#' @export
miter_fit.nested_cv <- function(data, workflow,
                                fitted_cv = NULL, resamples = NULL,
                                log_env = rlang::env(),
                                metric = metric_set(rmse),
                                grid = 10,
                                control = control_miter(),
                                param_info = NULL, ...) {

  control_inner <- control
  control_inner$progress <- FALSE

  out <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      fitted_inner = list(
        miter_fit.rset(
          inner_resamples,
          workflow,
          fitted_cv, resamples,
          log_env, metric,
          grid, control = control_inner,
          param_info, ... 
        )
      ),
      tuned_workflows = list(
        tune::finalize_workflow(workflow, tune::select_best(fitted_inner))
      ),
      fitted_outer =
        list(
          suppressMessages(
            fit(tuned_workflows, rsample::training(splits), ...)
          )
        )
    ) %>%
    dplyr::ungroup() %>%
    rsample:::add_class("nested_results")

  if (control$progress) {
    try(cli::cli_progress_update(.envir = log_env), silent = TRUE)
  }
  
  out
}
