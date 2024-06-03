#' Create a miter table by combining data with a list of workflows
#'
#' @param x A data frame or a miter table containing all relevant variables.
#' @param workflows A `workflow` object or a list containing `workflow` objects.
#'
#' @return A nested tibble with extra class `miter_tbl` and the following added
#' columns:
#'
#' * `models` contains character strings that identify the models provided in the `workflows` argument.
#' * `workflows` contains the workflows objects provided in the argument `workflows`
#' @export
add_workflows <- function(x, workflows) {
  UseMethod("add_workflows")
}

#' @export
add_workflows.data.frame <- function(x, workflows) {
  if (inherits(workflows, "workflow")) {
    workflows <- list(workflows)
  }

  wflow_names <- check_workflows(workflows)

  x <- as_miter_table(x)
  ids <- attr(x, "ids")
  classes <- attr(x, "class")

  try_overwriting <- tryCatch(
    x <- x %>%
      dplyr::select(-models, -workflows) %>%
      dplyr::distinct(),
    error = function(e) e
  )

  if (!inherits(try_overwriting, "error")) {
    rlang::warn("Existing workflows have been overwritten.")
  }

  wflows_list <- workflows[sapply(workflows, function(x) class(x) != "workflow")]

  x_out <- x %>%
    tidyr::expand_grid(
      tibble::tibble(
        models = wflow_names,
        workflows = workflows
      )
    )

  if (length(wflows_list) != 0) {
    x_out <- x_out %>%
      dplyr::left_join(
        purrr::map2_dfr(
          wflows_list,
          names(wflows_list),
          function(y, z) x %>%
                         dplyr::mutate(models = z, workflows_new = y) %>%
                         dplyr::select(-data)
        ),
        by = c(ids, "models")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(is_null = ifelse(is.null(workflows_new), 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        workflows = ifelse(is_null == 1, workflows, workflows_new)
      ) %>%
      dplyr::select(-workflows_new, -is_null)
  }

  if (!is.null(ids)) {
    x_out <- x_out %>%
      dplyr::relocate(models, .after = dplyr::all_of(ids))
  } else {
    x_out <- x_out %>%
      dplyr::relocate(models, .before = dplyr::everything())
  }

  attr(x_out, "ids") <- ids
  class(x_out) <- classes

  x_out
}
