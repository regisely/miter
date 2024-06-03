#' @importFrom yardstick metric_set rmse
#' @export
calculate_metrics <- function(object,
                              column = NULL,
                              metric = metric_set(rmse),
                              resamples = FALSE,
                              select_best = FALSE) {
  UseMethod("calculate_metrics")
}

#' @export
print.miter_metrics <- function(x, ...) {
  cat("# Miter Metrics\n")
  class(x) <- class(x)[!(class(x) %in% c("miter_metrics"))]
  print(x, ...)
}

#' @export
calculate_metrics.default <- function(object,
                              column = NULL,
                              metric = metric_set(rmse),
                              resamples = FALSE,
                              select_best = FALSE) {
  rlang::abort("Argument `object` should have class `miter_tbl`, `miter_pred` or `miter_ensemble`.")
}

#' @export
calculate_metrics.miter_pred <- function(object,
                                         column = NULL,
                                         metric = metric_set(rmse),
                                         resamples = FALSE,
                                         select_best = FALSE) {
  if (!is.null(column)) {
    rlang::warn("Ignoring argument `column`.")
  }
  outcome_var <- attr(object, "outcome")
  ids <- attr(object, "ids")

  ## MAKE A CHECK FOR RESAMPLES
  if (resamples) {
    out <-  object %>%
      tidyr::drop_na(.pred) %>%
      dplyr::group_by(across({{ ids }}), models, .id_resamples) %>%
      metric(!!sym(outcome_var), .pred) %>%
      dplyr::mutate(n = dplyr::n_distinct(.id_resamples)) %>%
      dplyr::group_by(across({{ ids }}), models) %>%
      dplyr::summarise(
               metric = mean(.estimate),
               std_err = sd(.estimate)/sqrt(mean(n)),
               .groups = "drop"
             ) %>%
      dplyr::group_by(across({{ ids }})) %>%
      dplyr::mutate(rank = dplyr::min_rank(metric)) %>%
      dplyr::ungroup()
  } else {
    out <- object %>%
      tidyr::drop_na(.pred) %>%
      dplyr::group_by(across({{ ids }}), models) %>%
      metric(!!sym(outcome_var), .pred) %>%
      dplyr::select(-.metric, -.estimator) %>%
      dplyr::rename(metric = .estimate) %>%
      dplyr::group_by(across({{ ids }})) %>%
      dplyr::mutate(rank = dplyr::min_rank(metric)) %>%
      dplyr::ungroup()
 }

  if (select_best) {
    out <- out %>%
      dplyr::filter(rank == 1) %>%
      dplyr::select(-rank)
      dplyr::arrange(across({{ ids }}), metric) %>%
      dplyr::group_by(across({{ ids }})) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }

  attr(out, "ids") <- ids
  attr(out, "metric") <- names(attributes(metric)$metrics)
  class(out) <- c("miter_metrics", class(out))
  out
}

#' @export
calculate_metrics.miter_tbl <- function(object,
                                        column = NULL,
                                        metric = metric_set(rmse),
                                        select_best = FALSE) {
  class_object <- class(object)
  arg <- rlang::enquo(column)
  arg_nm <- rlang::quo_name(arg)
  check_metrics_arg(object, arg_nm)
  col_classes <- class(purrr::pluck(object, arg_nm, 1))

  pred_tbl <- object %>%
    predict(!!arg, bind = "outcome")
  outcome_var <- attr(pred_tbl, "outcome")
  ids <- attr(pred_tbl, "ids")

  out <-  pred_tbl %>%
    dplyr::group_by(across({{ ids }}), models, .id_resamples) %>%
    metric(!!sym(outcome_var), .pred) %>%
    dplyr::mutate(n = dplyr::n_distinct(.id_resamples)) %>%
    dplyr::group_by(across({{ ids }}), models) %>%
    dplyr::summarise(
             metric = mean(.estimate),
             std_err = sd(.estimate)/sqrt(mean(n)),
             .groups = "drop"
           ) %>%
    dplyr::group_by(across({{ ids }})) %>%
    dplyr::mutate(rank = dplyr::min_rank(metric)) %>%
    dplyr::ungroup()

  if (select_best) {
    out <- out %>%
      dplyr::filter(rank == 1) %>%
      dplyr::select(-rank) %>%
      dplyr::arrange(across({{ ids }}), metric) %>%
      dplyr::group_by(across({{ ids }})) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }

  attr(out, "ids") <- ids
  attr(out, "metric") <- names(attributes(metric)$metrics)
  class(out) <- c("miter_metrics", class(out))
  out
}

#' @export
calculate_metrics.miter_ensemble <- function(object,
                                             column = NULL,
                                             metric = metric_set(rmse),
                                             select_best = FALSE) {
  class_object <- class(object)
  arg <- rlang::enquo(column)
  arg_nm <- rlang::quo_name(arg)
  check_metrics_arg(object, arg_nm)
  col_classes <- class(purrr::pluck(object, arg_nm, 1))

  pred_tbl <- object %>%
    predict.miter_tbl(!!arg, bind = "outcome")
  outcome_var <- attr(pred_tbl, "outcome")
  ids <- attr(pred_tbl, "ids")

  out <-  pred_tbl %>%
    dplyr::group_by(across({{ ids }}), models, .id_resamples) %>%
    metric(!!sym(outcome_var), .pred) %>%
    dplyr::mutate(n = dplyr::n_distinct(.id_resamples)) %>%
    dplyr::group_by(across({{ ids }}), models) %>%
    dplyr::summarise(
             metric = mean(.estimate),
             std_err = sd(.estimate)/sqrt(mean(n)),
             .groups = "drop"
           ) %>%
    dplyr::group_by(across({{ ids }})) %>%
    dplyr::mutate(rank = dplyr::min_rank(metric)) %>%
    dplyr::ungroup()

  if (select_best) {
    out <- out %>%
      dplyr::filter(rank == 1) %>%
      dplyr::select(-rank) %>%
      dplyr::arrange(across({{ ids }}), metric) %>%
      dplyr::group_by(across({{ ids }})) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }

  attr(out, "ids") <- ids
  attr(out, "metric") <- names(attributes(metric)$metrics)
  class(out) <- c("miter_metrics", class(out))
  out
}
