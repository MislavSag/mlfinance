#' SPY market data
#'
#' Market data for SPY with 5 minute frequrncy for period from 2018-01-01 to 2020-31-12.
#' Data was generated through Interactive Brokers API.
#'
#' @docType data
#'
#' @usage data(spy)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{open}{Open price}
#'  \item{high}{High price}
#'  \item{low}{Low price}
#'  \item{close}{Close price}
#'  \item{volume}{Volumme}
#'  \item{average}{Average price}
#'  \item{barcount}{Number of low frequent bars}
#' }
#' @references Data was collected using Interactive Brokers API
#' @keywords datasets
#' @examples
#'
#' data(spy)
#' head(spy)
#'
"spy"
