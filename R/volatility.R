#' @title daily_volatility
#'
#' @description Advances in Financial Machine Learning, Snippet 3.1, page 44.
#'  Computes the daily volatility at intraday estimation points.
#'
#' @param price xts object with prices or data.table object with columns index and price
#' @param span used to calculate alpha parameter in exponential moving average
#'
#' @return data.table with inde and daily_vol columns
#'
#' @examples
#' data("spy")
#' daily_volatility(spy[, c('index', 'close')])
#'
#' @export
#' @import data.table
#' @import lubridate
#' @import xts
#' @importFrom lubridate days
#' @importFrom xts is.xts


daily_volatility <- function(price, span = 50) {

  # convert to data.table because I will use DT merge with rolling
  if (xts::is.xts(price) | is.data.frame(price)) {
    price <- data.table::as.data.table(price)
  }

  # 1 day delta
  t_day_lag <- data.table::data.table(index = price$index - lubridate::days(1))

  print(t_day_lag)
  print(price)
  # merge t_day_lag with input index to get first and last period of daily vol
  x <- price[t_day_lag, on = 'index', roll = -Inf]
  x_ind <- price[t_day_lag, on = 'index', roll = -Inf, which = TRUE]

  # calculate returns of daily observations
  y = (x$close / price$close) - 1

  # calculate alpha based on span argument
  alpha <- 2 / (span + 1)

  # calculate ewm sd
  y <- slider::slide(.x = as.vector(y),
                     .f = ~{ewmsd(.x, alpha)}, .before = 1000, .complete = FALSE)
  y <- unlist(y)
  dt <- data.table(index = price$index, daily_vol = y, key = 'index')
  dt <- dt[x_ind > 1]
  return(dt)
}


#' @title ewmsd
#'
#' @description Exponential moving average standard devation
#'
#' @param y prices
#' @param span used to calculate alpha parameter in exponential moving average
#'
#' @return ewm standard devation
#'
#' @examples
#' data("spy")
#' daily_volatility(spy[, c('index', 'close')])

ewmsd <- function(y, alpha) {
   m <- length(y)
   weights <- (1 - alpha)^((m - 1):0)
   ewma <- sum(weights * y) / sum(weights)
   bias <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
   ewmsd <- sqrt(bias * sum(weights * (y - ewma)^2) / sum(weights))
   ewmsd
}

