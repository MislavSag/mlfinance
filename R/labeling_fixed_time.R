#' @title labeling_fixed_time
#'
#' @description Labeling time series data on fixed horizont
#'   Originally described in the book Advances in Financial Machine Learning, Chapter 3.2, p.43-44.
#'
#' @param prices vector of prices
#' @param threshold double, if return greater (lower )than threshold than  1 (-1), other 0
#' @param horizont integer, number of steps to the future
#'
#' @return vector of labels
#'
#' @import data.table
#'
#' @examples
#' data("spy")
#' head(labeling_fixed_time(spy$close, 0.05, 10000))
#'
#' @export
labeling_fixed_time <- function(prices, threshold=0.01, horizont=1L) {

  # checks
  checkmate::assert_int(horizont)

  # calculate future returns
  returns = shift(prices, n = horizont, type = 'lead') / prices - 1

  # labeling
  labels <- rep(0, length(returns))
  labels[returns < -threshold] <- -1
  labels[returns > threshold] <- 1

  return(labels)
}

