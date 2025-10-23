#' Brazilian ICMS Tax Revenue Data
#'
#' @description
#' Monthly time series data of ICMS (Imposto sobre Circulação de Mercadorias e Serviços)
#' tax revenue for Brazilian states. ICMS is a value-added tax levied by Brazilian
#' states on the circulation of goods and services.
#'
#' @format A data frame with 1404 rows and 3 variables:
#' \describe{
#'   \item{uf}{Character. Brazilian state abbreviation (UF - Unidade Federativa)}
#'   \item{date}{Date. Month and year of observation}
#'   \item{value}{Numeric. ICMS tax revenue value}
#' }
#'
#' @source Data compiled from Brazilian state tax authorities.
#'
#' @examples
#' data(icms_br)
#' head(icms_br)
#'
#' # View unique states
#' unique(icms_br$uf)
#'
#' # Time range
#' range(icms_br$date)
#'
"icms_br"
