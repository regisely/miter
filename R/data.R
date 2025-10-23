#' Brazilian Economic Indicators Data
#'
#' @description
#' Monthly time series data of economic indicators for Brazilian states including
#' ICMS tax revenue, IBC-Br economic activity index, and IPCA inflation.
#'
#' @format A data frame with 1404 rows and 5 variables:
#' \describe{
#'   \item{state}{Character. Brazilian state abbreviation}
#'   \item{date}{Date. Month and year of observation}
#'   \item{icms}{Numeric. ICMS (state VAT) tax revenue}
#'   \item{ibcr}{Numeric. IBC-Br (Central Bank Economic Activity Index)}
#'   \item{ipca}{Numeric. IPCA (Consumer Price Index) inflation rate}
#' }
#'
#' @source Data compiled from Brazilian economic databases.
#'
#' @examples
#' data(icms_br)
#' head(icms_br)
#'
#' # View unique states
#' unique(icms_br$state)
#'
#' # Time range
#' range(icms_br$date)
#'
"icms_br"
