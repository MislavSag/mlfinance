#' @title Auto Arima and Nnetar Forecasts
#'
#' @description Function calculates forecastas based on auto.arima and nnetar functions from forecast package.
#'
#' @param prices_panel a data.table object with coluimns: symbol, datetime, open, hogh, low, close, volume
#' @param row_index row index for which to calculate radf values.
#' @param windows Length of window for calculating forecasts on rolling window.
#' @param workers Number of workers for parallel processing
#' @param forecast_type type of time series forecasts.
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import checkmate
#' @import forecast
#' @import doParallel
#' @import runner
#' @importFrom parallel makeCluster clusterExport clusterCall stopCluster
#' @importFrom stats sd
#'
#' @export
rolling_univariate_forecasts <- function(prices_panel, row_index = 1:nrow(prices_panel), windows = c(200), workers = 4L,
                                    forecast_type = c("autoarima", "nnetar")) {

  # solve No visible binding for global variable
  symbol = open = high = low = close = volume = prices = returns_1 = `.` = returns = NULL

  # checks
  testSubset(c("symbol", "close"), colnames(prices_panel))
  assert_double(prices_panel$close, lower = 1e-005)

  # prepare data
  prices_panel[, returns := close / shift(close) - 1]
  prices_sample <- prices_panel[, .(symbol, date, returns)]

  # start cluster
  cl <- makeCluster(workers)
  clusterExport(cl, "prices_sample", envir = environment())
  clusterCall(cl, function() library(forecast))
  clusterCall(cl, function() library(data.table))

  # rolling arima forecasts
  data_list <- list()
  for (i in 1:length(windows)) {
    arima_forecasts <- runner(
      x = prices_sample,
      f = function(x) {

        # check if there is enough data
        if (length(unique(x$symbol)) > 1) {
          print(paste0("not enough data for symbol ", x$symbol[1]))
          return(NA)
        }

        # calculate arima forecasts
        if (forecast_type == "autoarima") {
          y <- auto.arima(x$returns)
          y <- as.data.table(forecast(y, 10))
          cols_prefix <- "autoarima_"
        } else if (forecast_type == "nnetar") {
          y <- nnetar(na.omit(x$returns))
          y <- as.data.table(forecast(y, PI = TRUE, h=22, npaths = 120))
          cols_prefix <- "nnetar_"
        }

        # clean arima forecasts
        first_forecasts <- y[1, ]
        colnames(first_forecasts) <- gsub(" ", "", paste0(cols_prefix, "1_", windows[i], "_", colnames(first_forecasts)))
        last_forecasts <- y[nrow(y), ]
        colnames(last_forecasts) <- gsub(" ", "", paste0(cols_prefix, "last_", windows[i], "_", colnames(last_forecasts)))
        mean_forecasts <- as.data.table(apply(y, 2, mean, na.rm = TRUE, simplify = FALSE))
        colnames(mean_forecasts) <- gsub(" ", "", paste0(cols_prefix, "mean_", windows[i], "_", colnames(mean_forecasts)))
        sd_forecasts <- as.data.table(apply(y, 2, sd, na.rm = TRUE, simplify = FALSE))
        colnames(sd_forecasts) <- gsub(" ", "", paste0(cols_prefix, "sd_", windows[i], "_", colnames(sd_forecasts)))
        data.table(symbol = x$symbol[1], date = x$date[length(x$date)], first_forecasts, last_forecasts, mean_forecasts, sd_forecasts)
      },
      k = windows[i],
      at = row_index,
      na_pad = TRUE,
      simplify = FALSE,
      cl = cl
    )
    gc()
    data_list[[i]] <- rbindlist(arima_forecasts[lengths(arima_forecasts) > 1L])
  }
  stopCluster(cl)
  data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)
  return(data_all_windows)
}
# prices_panel <- ohlcv[1:1000]
# x <- rolling_arima_forecasts(ohlcv[1:250], windows = c(100,200), workers = 4L)
