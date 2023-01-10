#' Create a nested table with extra class `miter_tbl`
#'
#' @param x A data frame containing all relevant variables.
#' @return A nested tibble with extra class `miter_tbl` and the following added
#' columns:
#'
#' * `id` could be one or more columns that identify unique ids provided by the
#' user through `group_by()`.
#' * `data` a list-column with the data provided by user in the argument `x` for
#' each specified group.
#' @export
#' @name miter_table
miter_table <- function(x) {
  as_miter_table(x)
}

#' @export
print.miter_tbl <- function(x, ...) {
  cat("# Miter Table\n")
  class(x) <- class(x)[!(class(x) %in% c("miter_tbl"))]
  print(x, ...)
}

#' @export
#' @rdname miter_table
as_miter_table <- function(x) {
  UseMethod("as_miter_table")
}

#' @export
as_miter_table.data.frame <- function(x) {
  has_list <- check_types(x, "list")
  has_data <- check_col_name(x, "data")

  if (!has_list) {
    x <- tidyr::nest(x, data = dplyr::everything())
  } else if (!has_data) {
    rlang::abort("List-columns not allowed before creating miter table.")
  } else {
    check_len_data(x)
  }

  attr(x, "ids") <- NULL
  class(x) <- c("miter_tbl", class(x))

  x
}

#' @export
as_miter_table.grouped_df <- function(x) {
  id_vars <- dplyr::group_vars(x)
  has_list <- check_types(x, "list")
  has_data <- check_col_name(x, "data")

  if (!has_list) {
    x <- x %>%
      tidyr::nest() %>%
      dplyr::ungroup()
  } else if (!has_data) {
    rlang::abort("List-columns not allowed before creating miter table.")
  } else {
    check_len_data(x)
    x <- x %>%
      dplyr::ungroup()
  }

  attr(x, "ids") <- id_vars
  class(x) <- c("miter_tbl", class(x))

  x
}

#' @export
as_miter_table.miter_tbl <- function(x) {
  x
}
