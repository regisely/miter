# GLOBAL VARIABLES
utils::globalVariables(
  c(
    "models", "workflows", ".data", ".pred", "splits", "predictions",
    "fitted_data", "data", "new_data", "workflows_new", "is_null",
    "metric", "std_err", ".id_resamples", ".estimate", "n", ".metric",
    ".estimator", ".config", "id", "fitted_inner", "fitted_outer",
    "inner_resamples", "tuned_workflows", ".id_inner", "resamples_data"
  )
)

# IMPORTS
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom rlang :=
#' @export
rlang::`:=`

#' @importFrom generics fit
#' @export
generics::fit

#' @importFrom dplyr across all_of any_of
#' @importFrom tidyr drop_na
#' @importFrom tidyselect where
#' @importFrom recipes step_rm
#' @importFrom tune collect_metrics
#' @importFrom stats as.formula sd
#' @importFrom grDevices hcl
