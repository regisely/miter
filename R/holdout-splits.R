#' Create holdout splits for each group
#'
#' @description
#' Creates holdout train/test splits for each group in a miter table using
#' `rsample::initial_split()`.
#'
#' @param x A data frame or miter table containing all relevant variables.
#' @param column The column containing the data to split. Default is `data`.
#' @param colname Name for the new column containing splits. Default is "splits".
#' @param prop The proportion of data to be retained for training. Default is 3/4.
#' @param strata A variable to use for stratified sampling.
#' @param breaks Number of bins for stratification. Default is 4.
#' @param pool The proportion of data used to determine splits. Default is 0.1.
#' @param ... Additional arguments passed to `rsample::initial_split()`.
#'
#' @return A miter table with an added column containing rsplit objects.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' icms_br %>%
#'   miter_table() %>%
#'   add_workflows(workflows) %>%
#'   holdout_split(prop = 0.8)
#' }
#'
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

  if (is.null(ids)) {
    args_by_id <- tibble::tibble(prop = prop[1])
    x <- x %>%
      dplyr::bind_cols(args_by_id)
  } else {
    args_by_id <- x %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
      dplyr::mutate(prop = rep_len(prop, dplyr::n()))
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

#' Create time-based holdout splits for each group
#'
#' @description
#' Creates time-based holdout train/test splits for each group in a miter table
#' using `rsample::initial_time_split()`.
#'
#' @param x A data frame or miter table containing time series data.
#' @param column The column containing the data to split. Default is `data`.
#' @param colname Name for the new column containing splits. Default is "splits".
#' @param prop The proportion of data to be retained for training. Default is 3/4.
#' @param lag A value to include a lag between training and testing. Default is 0.
#' @param ... Additional arguments passed to `rsample::initial_time_split()`.
#'
#' @return A miter table with an added column containing rsplit objects.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' icms_br %>%
#'   miter_table() %>%
#'   add_workflows(workflows) %>%
#'   holdout_time_split(prop = 0.8)
#' }
#'
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

  if (is.null(ids)) {
    args_by_id <- tibble::tibble(prop = prop[1], lag = lag[1])
    x <- x %>%
      dplyr::bind_cols(args_by_id)
  } else {
    args_by_id <- x %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
      dplyr::mutate(
               prop = rep_len(prop, dplyr::n()),
               lag = rep_len(lag, dplyr::n())
             )
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
