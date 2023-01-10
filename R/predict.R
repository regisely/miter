## Check missing and default arguments
## Deal with series with predictors
## Do an external check errors function for fit
## Reject predict on resamples
## Accept data mask in new_data argument (check predict on ensemble)
## Deal with duplicated resamples predictions  (param summarize of collect_predictions?)
## Leave parameters in resamples predictions?
#' @importFrom stats predict
#' @export
predict.miter_tbl <- function(object,
                              column,
                              new_data = NULL,
                              bind = "outcome", ...) {
  ids <- attr(object, "ids")
  classes <- attr(object, "class")

  arg <- rlang::enquo(column)
  arg_nm <- rlang::quo_name(arg)

  data_var <- check_predict_arg(object, arg_nm)

  errors <- which(
    purrr::map_chr(
      dplyr::pull(object, {{ arg }}), ~ class(.x)[1]
    ) == "try-error"
  )

  ### Make a check function
  if (is.null(new_data)) {
    if (arg_nm == "fitted_data") {
      rlang::abort("Argument `new_data` required to predict on full data.")
    }
  } else {
    if (arg_nm %in% c("fitted_resamples_data", "fittted_resamples_train",
                      "fitted_nested_cv_data")) {
      rlang::warn("Ignoring argument `new_data`.")
    }

    new_data <- check_arg_len(object, new_data)

    args_by_id <- object %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
      dplyr::mutate(
               new_data = rep_len(new_data, dplyr::n())
             )

    if (is.null(ids)) {
      object <- object %>%
        dplyr::bind_cols(args_by_id)
    } else {
      object <- object %>%
        dplyr::left_join(args_by_id, by = ids)
    }
  }

  if (!bind %in% c("none", "predictors", "outcome", "all")) {
    rlang::abort("Invalid argument for `bind`.")
  }

  out <- object %>%
    dplyr::filter(!dplyr::row_number() %in% errors) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      predictions = list(
        try(
          miter_predict(
            !!arg, .data[[data_var]],
            new_data = new_data,
            bind = bind, ...
          ),
          silent = TRUE
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(ids), models, predictions) %>%
    dplyr::filter(!purrr::map_lgl(predictions, ~ inherits(.x, "try-error"))) %>%
    tidyr::unnest(predictions) %>%
    dplyr::group_by(across({{ ids }})) %>%
    dplyr::filter(!(models != models[1L] & is.na(.pred))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(models = ifelse(is.na(.pred), "ACTUAL", models))

  var_roles <- object$workflows[[1]]$pre$actions$recipe$recipe$var_info
  outcome_var <- var_roles$variable[var_roles$role == "outcome"]

  attr(out, "outcome") <- outcome_var
  attr(out, "ids") <- ids
  class(out) <- c("miter_pred", class(out))
  out
}

#' @export
print.miter_pred <- function(x, ...) {
  cat("# Miter Predictions\n")
  class(x) <- class(x)[!(class(x) %in% c("miter_pred"))]
  print(x, ...)
}

#' @export
as_miter_pred <- function(x, ids, outcome) {
  UseMethod("as_miter_pred")
}

#' @export
as_miter_pred.data.frame <- function(x, ids, outcome) {
  attr(x, "outcome") <- outcome
  attr(x, "ids") <- ids
  class(x) <- c("miter_pred", class(x))
  x
}

#' @export
miter_predict <- function(object,
                          data,
                          new_data = NULL,
                          bind = "outcome", ...) {
  UseMethod("miter_predict", data)
}

#' @export
miter_predict.data.frame <- function(object,
                                     data,
                                     new_data,
                                     bind = "outcome", ...) {
  out <- predict(object, new_data = new_data, ...)

  if (bind == "all") {
    out <- dplyr::bind_rows(data, dplyr::bind_cols(out, new_data))
  } else if (bind == "predictors") {
    out <- dplyr::bind_cols(new_data, out)
  } else if (bind == "outcome") {
    outcome_var <- names(object$pre$mold$outcomes)
    out <- dplyr::bind_cols(dplyr::select(new_data, outcome_var), out)
  }

  out
}

#' @export
miter_predict.rsplit <- function(object,
                                 data,
                                 new_data = NULL,
                                 bind = "outcome", ...) {
  train <- rsample::training(data)

  if (is.null(new_data)) {
    new_data <- rsample::testing(data)
  }

  out <- predict(object, new_data = new_data, ...)

  if (bind == "all") {
    out <- dplyr::bind_rows(train, dplyr::bind_cols(out, new_data))
  } else if (bind == "predictors") {
    out <- dplyr::bind_cols(new_data, out)
  } else if (bind == "outcome") {
    outcome_var <- names(object$pre$mold$outcomes)
    out <- dplyr::bind_cols(dplyr::select(new_data, outcome_var), out)
  }

  out
}

#' @export
miter_predict.rset <- function(object,
                               data,
                               new_data = NULL,
                               bind = "outcome", ...) {
  all_data <- data$splits[[1]]$data %>%
    dplyr::mutate(.row = dplyr::row_number())

  best_parameters <- tune::select_best(object) %>%
    dplyr::pull(.config)

  out <- tune::collect_predictions(object) %>%
    dplyr::filter(.config == best_parameters) %>%
    dplyr::select(!any_of(names(object$.metrics[[1]]))) %>%
    dplyr::relocate(.pred, .after = tidyselect::last_col())

  if (bind == "all") {
    out <- all_data %>%
      dplyr::full_join(
               out %>%
               dplyr::select(id, .pred, .row),
               by = ".row"
             )
  } else if (bind == "predictors") {
    out <- all_data %>%
      dplyr::right_join(
               out %>%
               dplyr::select(id, .pred, .row),
               by = ".row"
             )
  } else if (bind == "none") {
    out <- out %>%
      dplyr::select(id, .pred, .row)
  }

  out <- out %>%
    dplyr::select(-.row) %>%
    dplyr::rename(.id_resamples = id) %>%
    dplyr::relocate(.id_resamples)
  
  out
}

#' @export
miter_predict.nested_cv <- function(object,
                                    data,
                                    new_data = NULL,
                                    bind = "outcome",
                                    inner = FALSE, ...) {

  var_roles <- object$fitted_outer[[1]]$pre$actions$recipe$recipe$var_info
  outcome_var <- var_roles$variable[var_roles$role == "outcome"]

  if (inner) {
    all_data <- rsample::training(data$splits[[1]]) %>%
      dplyr::mutate(.row = dplyr::row_number())

    out <- purrr::map2_dfr(object$id, object$fitted_inner, function(x, y) {
      best_parameters <- tune::select_best(y) %>%
        dplyr::pull(.config)

      tune::collect_predictions(y) %>%
        dplyr::filter(.config == best_parameters) %>%
        dplyr::mutate(.id_resamples = x) %>%
        dplyr::rename(.id_inner = id) %>%
        dplyr::select(.id_resamples, .id_inner, .pred, .row)
    })
  } else {
    all_data <- data$splits[[1]]$data %>%
      dplyr::mutate(.row = dplyr::row_number())

    out <- object %>%
      dplyr::rowwise() %>%
      dplyr::transmute(
               predictions = list(
                 predict(fitted_outer, rsample::testing(splits)) %>%
                 dplyr::mutate(
                          .id_resamples = id,
                          .row = dplyr::row_number() +
                            nrow(rsample::training(splits))
                        )
               )
             ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = predictions)
  }

  if (bind == "all") {
    out <- all_data %>%
      dplyr::full_join(
               out,
               by = ".row"
             )
  } else if (bind == "predictors") {
    out <- all_data %>%
      dplyr::right_join(
               out,
               by = ".row"
             )
  } else if (bind == "outcome") {
    out <- all_data %>%
      dplyr::select(outcome_var, .row) %>%
      dplyr::right_join(
               out,
               by = ".row"
             )
  }

  if (inner) {
    out <- out %>%
      dplyr::select(-.row) %>%
      dplyr::relocate(.id_resamples, .id_inner) %>%
      dplyr::arrange(
               !is.na(.pred),
               dplyr::desc(.id_resamples),
               dplyr::desc(.id_inner)
             )
  } else {
    out <- out %>%
      dplyr::select(-.row) %>%
      dplyr::relocate(.id_resamples) %>%
      dplyr::arrange(!is.na(.pred), dplyr::desc(.id_resamples))
  }

  out
}
