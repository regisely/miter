#' Create cross-validation resamples for each group
#'
#' @description
#' Creates V-fold cross-validation resamples for each group in a miter table
#' using `rsample::vfold_cv()`.
#'
#' @param x A data frame or miter table.
#' @param column The column containing the data to resample. Default is `splits`.
#' @param colname Name for the new column containing resamples. Default is NULL.
#' @param v Number of folds for cross-validation. Default is 5.
#' @param repeats Number of times to repeat the V-fold cross-validation. Default is 1.
#' @param strata A variable to use for stratified sampling.
#' @param breaks Number of bins for stratification. Default is 4.
#' @param pool The proportion of data used to determine splits. Default is 0.1.
#'
#' @return A miter table with an added column containing rset objects.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' icms_br %>%
#'   miter_table() %>%
#'   add_workflows(workflows) %>%
#'   holdout_split(prop = 0.8) %>%
#'   cv_split(v = 5)
#' }
#'
#' @export
cv_split <- function(x,
                     column = splits,
                     colname = NULL,
                     v = 5,
                     repeats = 1,
                     strata = NULL,
                     breaks = 4,
                     pool = 0.1) {
  UseMethod("cv_split")
}

#' @export
cv_split.data.frame <- function(x,
                                column = splits,
                                colname = NULL,
                                v = 5,
                                repeats = 1,
                                strata = NULL,
                                breaks = 4,
                                pool = 0.1) {
  x <- as_miter_table(x)
  ids <- attr(x, "ids")
  classes <- attr(x, "class")

  arg <- rlang::enquo(column)
  arg <- check_column_arg(x, arg)
  arg_nm <- rlang::quo_name(arg)
  arg_class <- class(purrr::pluck(x, arg_nm, 1))
  if ("rsplit" %in% arg_class | "mc_split" %in% arg_class) {
    arg <- rlang::set_expr(arg, rlang::expr(rsample::training(!!arg)))
  }
  if (is.null(colname)) {
    colname <- gsub("splits", "train", glue::glue("resamples_{arg_nm}"))
  }
  v <- check_arg_len(x, v)
  repeats <- check_arg_len(x, repeats)
  strata <- check_arg_len(x, strata)
  breaks <- check_arg_len(x, breaks)
  pool <- check_arg_len(x, pool)

  if (is.null(ids)) {
    args_by_id <- tibble::tibble(
      v = v[1],
      repeats = repeats[1],
      strata = if (!is.null(strata)) strata[1] else NULL,
      breaks = breaks[1],
      pool = pool[1]
    )
    x <- x %>%
      dplyr::bind_cols(args_by_id)
  } else {
    args_by_id <- x %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
      dplyr::mutate(
        v = rep_len(v, dplyr::n()),
        repeats = rep_len(repeats, dplyr::n()),
        strata = if (!is.null(strata)) rep_len(strata, dplyr::n()) else NULL,
        breaks = rep_len(breaks, dplyr::n()),
        pool = rep_len(pool, dplyr::n())
      )
    x <- x %>%
      dplyr::left_join(args_by_id, by = ids)
  }

  out <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
             "{colname}" := list(
               rsample::vfold_cv(
                 !!arg,
                 v = if (is.function(v)) v(!!arg) else v,
                 repeats = if (is.function(repeats)) repeats(!!arg) else repeats,
                 strata = strata,
                 breaks = if (is.function(breaks)) breaks(!!arg) else breaks,
                 pool = if (is.function(pool)) pool(!!arg) else pool
               )
             )
           ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-v, -repeats, -strata, -breaks, -pool)

  attr(out, "ids") <- ids
  class(out) <- classes

  out
}

#' Create time-based cross-validation resamples for each group
#'
#' @description
#' Creates time-based rolling origin resamples for each group in a miter table
#' using the internal `rolling_cv()` function.
#'
#' @param x A data frame or miter table containing time series data.
#' @param column The column containing the data to resample. Default is `splits`.
#' @param colname Name for the new column containing resamples. Default is NULL.
#' @param initial Number of samples in the initial training set. Can be a function.
#'   Default is a function that returns 2/3 of the data.
#' @param assess Number of samples in the assessment set. Can be a function.
#'   Default is a function that returns 1/15 of the data.
#' @param cumulative Logical. Should the training set grow with each resample? Default is TRUE.
#' @param skip Number of samples to skip between resamples. Default is `assess`.
#' @param lag A value to include a lag between training and testing. Default is 0.
#' @param slice_limit Maximum number of resamples to create. Default is Inf.
#'
#' @return A miter table with an added column containing rset objects.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' icms_br %>%
#'   miter_table() %>%
#'   add_workflows(workflows) %>%
#'   holdout_time_split(prop = 0.8) %>%
#'   cv_time_split(slice_limit = 5)
#' }
#'
#' @export
cv_time_split <- function(x,
                          column = splits,
                          colname = NULL,
                          initial = function(x) round(nrow(x) * 2 / 3),
                          assess = function(x) floor(nrow(x) * 1 / 15),
                          cumulative = TRUE,
                          skip = assess,
                          lag = 0,
                          slice_limit = Inf) {
  UseMethod("cv_time_split")
}

#' @export
cv_time_split.data.frame <- function(x,
                                     column = splits,
                                     colname = NULL,
                                     initial = function(x) round(nrow(x) * 2/3),
                                     assess = function(x) floor(nrow(x) * 1/15),
                                     cumulative = TRUE,
                                     skip = assess,
                                     lag = 0,
                                     slice_limit = Inf) {
  x <- as_miter_table(x)
  ids <- attr(x, "ids")
  classes <- attr(x, "class")

  arg <- rlang::enquo(column)
  arg <- check_column_arg(x, arg)
  arg_nm <- rlang::quo_name(arg)
  arg_class <- class(purrr::pluck(x, arg_nm, 1))[1]
  if ("rsplit" %in% arg_class | "initial_split" %in% arg_class) {
    arg <- rlang::set_expr(arg, rlang::expr(rsample::training(!!arg)))
  }
  if (is.null(colname)) {
    colname <- gsub("splits", "train", glue::glue("resamples_{arg_nm}"))
  }
  initial <- check_arg_len(x, initial)
  assess <- check_arg_len(x, assess)
  skip <- check_arg_len(x, skip)
  check_arg_len(x, cumulative)
  check_arg_len(x, lag)
  check_arg_len(x, slice_limit)

  if (is.null(ids)) {
    args_by_id <- tibble::tibble(
      initial = initial[1],
      assess = assess[1],
      cumulative = cumulative[1],
      skip = skip[1],
      lag = lag[1],
      slice_limit = slice_limit[1]
    )
    x <- x %>%
      dplyr::bind_cols(args_by_id)
  } else {
    args_by_id <- x %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
      dplyr::mutate(
        initial = rep_len(initial, dplyr::n()),
        assess = rep_len(assess, dplyr::n()),
        cumulative = rep_len(cumulative, dplyr::n()),
        skip = rep_len(skip, dplyr::n()),
        lag = rep_len(lag, dplyr::n()),
        slice_limit = rep_len(slice_limit, dplyr::n())
      )
    x <- x %>%
      dplyr::left_join(args_by_id, by = ids)
  }

  out <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
             "{colname}" := list(
               rolling_cv(
                 !!arg,
                 initial = if (is.function(initial)) initial(!!arg) else initial,
                 assess = if (is.function(assess)) assess(!!arg) else assess,
                 cumulative = cumulative,
                 skip = if (is.function(skip)) skip(!!arg) else skip,
                 lag = lag,
                 slice_limit = slice_limit
               )
             )
           ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-initial, -assess, -cumulative, -skip, -lag, -slice_limit)

  attr(out, "ids") <- ids
  class(out) <- classes

  out
}

#' Create nested time-based cross-validation resamples for each group
#'
#' @description
#' Creates nested time-based cross-validation resamples for each group in a miter table.
#' The outer resamples are used for model assessment and the inner resamples are used
#' for tuning.
#'
#' @param x A data frame or miter table containing time series data.
#' @param column The column containing the data to resample. Default is `splits`.
#' @param colname Name for the new column containing nested resamples. Default is NULL.
#' @param outside A function to create outer resamples. Default uses `rolling_cv()`
#'   with 1/30 of data for assessment and a limit of 10 slices.
#' @param inside A function to create inner resamples. Default uses `rolling_cv()`
#'   with a limit of 5 slices.
#'
#' @return A miter table with an added column containing nested rset objects.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' icms_br %>%
#'   miter_table() %>%
#'   add_workflows(workflows) %>%
#'   holdout_time_split(prop = 0.8) %>%
#'   nested_cv_time_split()
#' }
#'
#' @export
nested_cv_time_split <- function(x,
                                 column = splits,
                                 colname = NULL,
                                 outside = function(x) {
                                   rolling_cv(x,
                                     assess = floor(nrow(x) * 1 / 30),
                                     slice_limit = 10
                                   )
                                 },
                                 inside = function(x) {
                                   rolling_cv(x, slice_limit = 5)
                                 }) {
  UseMethod("nested_cv_time_split")
}

#' @export
nested_cv_time_split.data.frame <- function(x,
                                            column = splits,
                                            colname = NULL,
                                            outside = function(x) {
                                              rolling_cv(
                                                x,
                                                assess = floor(nrow(x) * 1 / 30),
                                                slice_limit = 10
                                              )
                                            },
                                            inside = function(x) {
                                              rolling_cv(x,
                                                slice_limit = 5
                                              )
                                            }) {
  cl <- match.call()
  x <- as_miter_table(x)
  ids <- attr(x, "ids")
  classes <- attr(x, "class")

  arg <- rlang::enquo(column)
  arg <- check_column_arg(x, arg)
  arg_nm <- rlang::quo_name(arg)
  arg_class <- class(purrr::pluck(x, arg_nm, 1))[1]
  if (arg_class == "rsplit") {
    arg <- rlang::set_expr(arg, rlang::expr(rsample::training(!!arg)))
  }
  if (is.null(colname)) {
    colname <- gsub("splits", "train", glue::glue("nested_cv_{arg_nm}"))
  }
  outside <- check_arg_len(x, outside)
  inside <- check_arg_len(x, inside)

  if (is.null(ids)) {
    args_by_id <- tibble::tibble(
      outside = outside[1],
      inside = inside[1]
    )
    x <- x %>%
      dplyr::bind_cols(args_by_id)
  } else {
    args_by_id <- x %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(ids))) %>%
      dplyr::mutate(
        outside = rep_len(outside, dplyr::n()),
        inside = rep_len(inside, dplyr::n())
      )
    x <- x %>%
      dplyr::left_join(args_by_id, by = ids)
  }

  out <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
             "{colname}" := list(
               dplyr::mutate(
                        outside(!!arg),
                        inner_resamples =
                          purrr::map(splits, ~ inside(as.data.frame(.x)))
                      ) %>%
               rsample:::add_class("nested_cv") %>%
               add_attr("outside", cl) %>%
               add_attr("inside", cl)
             )
           ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-outside, -inside)

  attr(out, "ids") <- ids
  class(out) <- classes

  out
}

#' Create rolling origin resamples for time series
#'
#' @description
#' Creates rolling origin resamples for time series cross-validation. This is
#' similar to `rsample::rolling_origin()` but with additional flexibility.
#'
#' @param data A data frame containing time series data.
#' @param initial Number of samples in the initial training set. Default is 2/3 of data.
#' @param assess Number of samples in the assessment set. Default is 1/15 of data.
#' @param cumulative Logical. Should the training set grow with each resample? Default is TRUE.
#' @param skip Number of samples to skip between resamples. Default is `assess`.
#' @param lag A value to include a lag between training and testing. Default is 0.
#' @param slice_limit Maximum number of resamples to create. Default is Inf.
#' @param ... Additional arguments (not currently used).
#'
#' @return An rset object containing rolling origin resamples.
#'
#' @examples
#' \dontrun{
#' data(icms_br)
#' icms_sample <- icms_br %>% dplyr::filter(state == "SP")
#' resamples <- rolling_cv(icms_sample, slice_limit = 5)
#' }
#'
#' @importFrom rsample make_splits
#' @importFrom rsample new_rset
#' @export
rolling_cv <- function(data,
                       initial = round(nrow(data) * 2 / 3),
                       assess = floor(nrow(data) * 1 / 15),
                       cumulative = TRUE,
                       skip = assess,
                       lag = 0, slice_limit = Inf, ...) {
  n <- nrow(data)
  if (n < initial + assess) {
    stop("There should be at least ", initial + assess, " nrows in `data`",
      call. = FALSE
    )
  }
  if (!is.numeric(lag) | !(lag %% 1 == 0)) {
    stop("`lag` must be a whole number.", call. = FALSE)
  }
  if (lag > initial) {
    stop("`lag` must be less than or equal to the number of training observations.",
      call. = FALSE
    )
  }
  stops <- n - seq(assess, (n - initial), by = skip)
  starts <- if (!cumulative) {
    stops - initial + 1
  } else {
    starts <- rep(1, length(stops))
  }
  in_ind <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  out_ind <- mapply(seq, stops + 1 - lag, stops + assess, SIMPLIFY = FALSE)
  indices <- mapply(rsample:::merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)
  split_objs <- purrr::map(indices, make_splits,
    data = data,
    class = "rof_split"
  )
  slice_limit <- min(length(split_objs), slice_limit)
  split_objs <- split_objs[1:slice_limit]
  split_objs <- list(splits = split_objs, id = rsample:::names0(
    length(split_objs),
    "Slice"
  ))
  roll_att <- list(
    initial = initial, assess = assess, cumulative = cumulative,
    skip = skip, lag = lag
  )
  new_rset(
    splits = split_objs$splits, ids = split_objs$id,
    attrib = roll_att, subclass = c("rolling_origin", "rset")
  )
}
