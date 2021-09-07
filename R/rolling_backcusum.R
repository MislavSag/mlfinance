#' @title Rolling BackCUSUM Tests
#'
#' @description Function calculates backCUSUM tests on rolling window from backCUSUM pakcage.
#'
#' @param prices_panel a data.table object with coluimns: symbol, datetime, open, hogh, low, close, volume
#' @param row_index row index for which to calculate radf values.
#' @param windows Length of window for calculating forecasts on rolling window.
#' @param workers Number of workers for parallel processing
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import checkmate
#' @import backCUSUM
#' @import doParallel
#' @import runner
#' @importFrom parallel makeCluster clusterExport clusterCall stopCluster
#' @importFrom stats as.formula
#'
#' @export
rolling_backcusum <- function(prices_panel,
                              row_index = 1:nrow(prices_panel),
                              windows = c(200),
                              workers = 4L) {

  # solve No visible binding for global variable
  symbol = close = `.` = returns = NULL

  # checks
  testSubset(c("symbol", "close"), colnames(prices_panel))
  assert_double(prices_panel$close, lower = 1e-005)

  # prepare data
  prices_panel[, returns := close / shift(close) - 1]
  prices_sample <- prices_panel[, .(symbol, date, returns)]

  # start cluster
  cl <- makeCluster(workers)
  clusterExport(cl, "prices_sample", envir = environment())
  clusterCall(cl, function() library(backCUSUM))
  clusterCall(cl, function() library(data.table))

  # rolling arima forecasts
  data_list <- list()
  for (i in 1:length(windows)) {
    forecasts <- runner(
      x = prices_sample,
      f = function(x) {

        # check if there is enough data
        if (length(unique(x$symbol)) > 1) {
          print(paste0("not enough data for symbol ", x$symbol[1]))
          return(NA)
        }

        # calculate arima forecasts
        y <- na.omit(x$returns)
        y <- SBQ.test(as.formula('y ~ 1'), alternative = 'greater')# [['statistic']]
        results <- c(y[['statistic']], as.integer(y[['rejection']]))
        names(results) <- c("statistics", paste0("backcusum_rejections_", as.numeric(names(y[['rejection']])) * 1000))
        results <- as.data.table(as.list(results))
        results <- data.table(symbol = x$symbol[1], date = x$date[length(x$date)], results)
        colnames(results)[3:ncol(results)] <- paste(colnames(results)[3:ncol(results)], windows[i], sep = "_")
        return(results)
      },
      k = windows[i],
      at = row_index,
      na_pad = TRUE,
      simplify = FALSE,
      cl = cl
    )
    gc()
    data_list[[i]] <- rbindlist(forecasts[lengths(forecasts) > 1L])
  }
  stopCluster(cl)
  data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)
  return(data_all_windows)
}
# delete later
# test <- rolling_backcusum(prices_panel[1:300],
#                   row_index = 1:nrow(prices_panel),
#                   windows = c(100, 200),
#                   workers = 4L)
