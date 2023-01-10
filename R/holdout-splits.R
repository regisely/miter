#' Create holdout splits for each group
#'
#' @param x A data frame or miter table containing all relevant variables
#' @param prop Description
#' @return A miter table with the following added columns:
#'
#' * `splits` a list of rsplit objects
#' @export
holdout_split <- function(x,
                          column = data,
                          colname = "splits",
                          prop = 3 / 4,
                          strata = NULL,
                          breaks = 4,
                          pool = 0.1, ...) {
  UseMethod("holdout_split")
}

#' @export
holdout_split.data.frame <- function(x,
                                     column = data,
                                     colname = NULL,
                                     prop = 3 / 4,
                                     strata = NULL,
                                     breaks = 4,
                                     pool = 0.1, ...) {
  x <- as_miter_table(x)
  ids <- attr(x, "ids")
  classes <- attr(x, "class")

  arg <- rlang::enquo(column)
  check_arg_len(x, prop)
  if (is.null(colname)) colname <- "splits"

  args_by_id <- x %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
    dplyr::mutate(prop = rep_len(prop, dplyr::n()))

  if (is.null(ids)) {
    x <- x %>%
      dplyr::bind_cols(args_by_id)
  } else {
    x <- x %>%
      dplyr::left_join(args_by_id, by = ids)
  }

  out <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "{colname}" := list(
        rsample::initial_split(!!arg, prop = prop, ...)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prop)

  attr(out, "ids") <- ids
  class(out) <- classes

  out
}

#' @export
holdout_time_split <- function(x,
                               column = data,
                               colname = "splits",
                               prop = 3 / 4,
                               lag = 0, ...) {
  UseMethod("holdout_time_split")
}

#' @export
holdout_time_split.data.frame <- function(x,
                                          column = data,
                                          colname = NULL,
                                          prop = 3 / 4,
                                          lag = 0, ...) {
  x <- as_miter_table(x)
  ids <- attr(x, "ids")
  classes <- attr(x, "class")

  arg <- rlang::enquo(column)
  check_arg_len(x, prop)
  check_arg_len(x, lag)
  if (is.null(colname)) colname <- "splits"

  args_by_id <- x %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
    dplyr::mutate(
             prop = rep_len(prop, dplyr::n()),
             lag = rep_len(lag, dplyr::n())
           )

  if (is.null(ids)) {
    x <- x %>%
      dplyr::bind_cols(args_by_id)
  } else {
    x <- x %>%
      dplyr::left_join(args_by_id, by = ids)
  }

  out <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "{colname}" := list(
        rsample::initial_time_split(!!arg, prop = prop, lag = lag, ...)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prop, -lag)

  attr(out, "ids") <- ids
  class(out) <- classes

  out
}
