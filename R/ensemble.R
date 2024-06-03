#' @export
create_ensemble <- function(data, workflows, n_models = NULL,
                            fit_resamples = FALSE,
                            metric = metric_set(rmse),
                            cv = NULL, ...) {
  check_predictions(data)
  ids <- attr(data, "ids")
  outcome_var <- attr(data, "outcome")

  cl <- match.call()
  cv_cl <- cl[["cv"]]
  if (rlang::is_call(cv_cl)) {
    cv_cl$column <- quote(data)
  } else if (is.null(cv_cl)) {
    cv_cl <- rlang::call2("cv_split")
    cv_cl$column <- quote(data)
  } else {
    rlang::abort("Argument `cv` should be a function.")
  }

  resamples <- ".id_resamples" %in% names(data)
  id_cols <- data %>%
    tidyr::pivot_wider(names_from = "models", values_from = ".pred") %>%
    dplyr::select(dplyr::all_of(ids))

  data <- data %>%
    dplyr::mutate(
      dplyr::across(ids, ~ factor(.x, levels = unique(.x)))
    )

  if (!is.null(n_models)) {
    data <- data %>%
      calculate_metrics(metric = metric, resamples = resamples) %>%
      dplyr::filter(rank %in% 1:n_models) %>%
      dplyr::select(dplyr::all_of(ids), models) %>%
      dplyr::left_join(data, by = c(ids, "models")) %>%
      tidyr::drop_na(.pred) %>%
      dplyr::select(
               dplyr::all_of(ids),
               models,
               dplyr::all_of(outcome_var),
               .pred
             ) %>%
      tidyr::pivot_wider(names_from = "models", values_from = ".pred")
  } else {
    data <- data %>%
      tidyr::drop_na(.pred) %>%
      dplyr::select(
               dplyr::all_of(ids),
               models,
               dplyr::all_of(outcome_var),
               .pred
             ) %>%
      tidyr::pivot_wider(names_from = "models", values_from = ".pred")
  }

  data_col <- data %>%
    dplyr::group_split(dplyr::across(ids), .keep = FALSE) %>%
    purrr::map(
      function(x) dplyr::select(x, tidyselect:::where(~!all(is.na(.x))))
    )

  data_workflows <- tidyr::crossing(data_col, workflows) 

  workflows_col <- purrr::map2(
    data_workflows$data_col, data_workflows$workflows,
    function(x, y) {
      mv_vars <- colnames(x)[which(colnames(x) != outcome_var)]
      formula_new <- as.formula(
        paste(outcome_var, "~", paste0("`", mv_vars, "`", collapse = " + "))
      )
      y %>%
        workflows::update_recipe(recipes::recipe(formula_new, data = x))
    })
    
  out <- data %>%
    dplyr::select(-all_of(ids)) %>%
    dplyr::bind_cols(id_cols, .) %>%
    dplyr::group_by(across({{ ids }})) %>%
    as_miter_table() %>%
    dplyr::mutate(data = data_col) %>%
    add_workflows(workflows) %>%
    dplyr::mutate(workflows = workflows_col)

  cv_cl$x <- quote(out)

  if (fit_resamples) {
    out <- eval(cv_cl) %>%
      fit(resamples_data, ...) %>%
      fit(data, ...)
  } else {
    out <- eval(cv_cl) %>%
      fit(data, ...)
  }

  class(out) <- c("miter_ensemble", class(out))
  out
}

#' @export
print.miter_ensemble <- function(x, ...) {
  cat("# Miter Ensemble\n")
  class(x) <- class(x)[!(class(x) %in% c("miter_ensemble"))]
  print(x, ...)
}

#' @export
add_ensemble <- function(object, preds, ...) {
  ids <- attr(object, "ids")
  classes <- attr(object, "class")

  check_predict_arg(object, "fitted_data")
  is_miter_pred <- "miter_pred" %in% class(preds)
  if (!is_miter_pred) {
    rlang::abort("Argument `preds` should have class `miter_pred`.")
  }

  errors <- which(
    purrr::map_chr(
      dplyr::pull(object, fitted_data), ~ class(.x)[1]
    ) == "try-error"
  )

  new_data_list <- preds %>%
    tidyr::drop_na(.pred) %>%
    tidyr::pivot_wider(names_from = "models", values_from = ".pred") %>%
    dplyr::group_by(across({{ ids }})) %>%
    tidyr::nest() %>%
    dplyr::pull(data)

  args_by_id <- object %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
    dplyr::mutate(new_data = rep_len(new_data_list, dplyr::n()))

  if (is.null(ids)) {
    object <- object %>%
      dplyr::bind_cols(args_by_id)
  } else {
    object <- object %>%
      dplyr::left_join(args_by_id, by = ids)
  }

  out <- object %>%
    dplyr::filter(!dplyr::row_number() %in% errors) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      predictions = list(
        try(
          miter_predict(
            fitted_data, data,
            new_data = new_data,
            bind = "predictors", ...
          ),
          silent = TRUE
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(ids), models, predictions) %>%
    dplyr::filter(!purrr::map_lgl(predictions, ~ inherits(.x, "try-error"))) %>%
    tidyr::unnest(predictions) %>%
    dplyr::select(all_of(colnames(preds))) %>%
    dplyr::bind_rows(preds, .) %>%
    dplyr::arrange(dplyr::across({{ ids }}))

  var_roles <- object$workflows[[1]]$pre$actions$recipe$recipe$var_info
  outcome_var <- var_roles$variable[var_roles$role == "outcome"]

  attr(out, "outcome") <- outcome_var
  attr(out, "ids") <- ids
  class(out) <- c("miter_pred", class(out))
  out
}
