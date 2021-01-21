#' @title daily_volatility
#'
#' @description Advances in Financial Machine Learning, Snippet 3.1, page 44.
#'  Computes the daily volatility at intraday estimation points.
#'
#' @param price xts object with prices or data.table object with first column is of type POSIXct
#' @param span used to calculate alpha parameter in exponential moving average
#'
#' @return data.table with Datetime and Value columns
#'
#' @import data.table
#' @importFrom lubridate days
#' @importFrom slider slide
#' @import checkmate
#'
#' @examples
#' data("spy")
#' daily_volatility(subset(spy, select = c("index", "close")))
#'
#' @export

daily_volatility <- function(price, span = 50) {

  # check argument types
  price <- two_column_check(price)
  checkmate::assert_number(span)

  # solve No visible binding for global variable
  Datetime <- Value <- NULL

  # 1 day delta
  t_day_lag <- data.table::data.table(Datetime = price$Datetime - days(1))

  # merge t_day_lag with input index to get first and last period of daily vol
  x <- price[t_day_lag, on = 'Datetime', roll = -Inf]
  x_ind <- price[t_day_lag, on = 'Datetime', roll = -Inf, which = TRUE]

  # calculate returns of daily observations
  y = (x$Value / price$Value) - 1

  # calculate alpha based on span argument
  alpha <- 2 / (span + 1)

  # calculate ewm sd
  y <- slide(.x = as.vector(y),
             .f = ~{ewmsd(.x, alpha)}, .before = 1000, .complete = FALSE)
  y <- unlist(y)
  dt <- data.table(Datetime = price$Datetime, Value = y, key = 'Datetime')
  dt <- dt[x_ind > 1]
  return(dt)
}


#' @title ewmsd
#'
#' @description Exponential moving average standard devation
#'
#' @param y prices
#' @param alpha smoothing factor
#'
#' @return ewm standard devation
#'
#' @examples
#' data("spy")
#' daily_volatility(subset(spy, select = c("index", "close")))

ewmsd <- function(y, alpha) {
   m <- length(y)
   weights <- (1 - alpha)^((m - 1):0)
   ewma <- sum(weights * y) / sum(weights)
   bias <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
   ewmsd <- sqrt(bias * sum(weights * (y - ewma)^2) / sum(weights))
   ewmsd
}

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
