#' @title Rolling catch 22
#'
#' @description Function calculates catch 22 features on rolling window.
#'
#' @param prices_panel a data.table object with coluimns: symbol, datetime, open, hogh, low, close, volume
#' @param windows Length of window for calculating forecasts on rolling window.
#' @param workers Number of workers for parallel processing
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import checkmate
#' @import forecast
#' @import doParallel
#' @import runner
#' @import Rcatch22
#'
#' @export
rolling_catch22 <- function(prices_panel, row_index = 1:nrow(prices_panel), windows = c(200), workers = 4L) {

  # solve No visible binding for global variable
  symbol <- open <- high <- low <- close <- volume <- prices <- returns_1 <- NULL

  # checks
  testSubset(c("symbol", "close"), colnames(prices_panel))
  assert_double(prices_panel$close, lower = 1e-005)

  # prepare data
  prices_sample <- prices_panel[, .(symbol, date, close)]

  # start cluster
  cl <- makeCluster(workers)
  clusterExport(cl, "prices_sample", envir = environment())
  clusterCall(cl, function() library(Rcatch22))
  clusterCall(cl, function() library(data.table))

  # rolling arima forecasts
  data_list <- list()
  for (i in 1:length(windows)) {
    rolling_data <- runner(
      x = prices_sample,
      f = function(x) {

        # check if there is enough data
        if (length(unique(x$symbol)) > 1) {
          print(paste0("not enough data for symbol ", x$symbol[1]))
          return(NA)
        }

        # calculate arima forecasts
        y <- catch22_all(x$close)
        catch22_features_ <- data.table::transpose(y, make.names = "names")
        results <- data.table(symbol = x$symbol[1], date = x$date[length(x$date)], catch22_features_)
        colnames(results)[3:ncol(results)] <- paste(colnames(results)[3:ncol(results)], windows[i], sep = "_")
        as.data.table(results)
      },
      k = windows[i],
      at = row_index,
      na_pad = TRUE,
      simplify = FALSE,
      cl = cl
    )
    gc()
    data_list[[i]] <- rbindlist(rolling_data[lengths(rolling_data) > 1L])
  }
  stopCluster(cl)
  data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)
  return(data_all_windows)
}
# prices_panel <- ohlcv[1:1000]
# x <- rolling_catch22(prices_panel, windows = 200, workers = 4L)
