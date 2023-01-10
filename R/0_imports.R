# GLOBAL VARIABLES
utils::globalVariables(
  c(
    "models", "workflows", ".data", ".pred", "splits", "predictions"
  )
)

# IMPORTS
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom rlang :=
#' @export
rlang::`:=`
