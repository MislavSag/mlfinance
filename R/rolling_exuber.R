#' @title Rolling Exuber
#'
#' @description Function calculates radf values from exuber package on rolling window.
#'
#' @param prices_panel a data.table object with columns: symbol, datetime, close.
#' @param row_index row index for which to calculate radf values.
#' @param windows Length of window for calculating forecasts on rolling window.
#' @param workers Number of workers for parallel processing
#' @param exuber_lag exuber lag.
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import checkmate
#' @import doParallel
#' @import runner
#' @importFrom exuber radf psy_minw augment tidy index
#'
#' @export
rolling_exuber <- function(prices_panel, row_index = 1:nrow(prices_panel), windows = c(200), workers = 4L, exuber_lag = 0L) {

  # solve No visible binding for global variable
  symbol <- close <- NULL

  # checks
  testSubset(c("symbol", "close"), colnames(prices_panel))
  assert_double(prices_panel$close, lower = 1e-005)

  # prepare data
  prices_sample <- prices_panel[, .(symbol, date, close)]

  # start cluster
  cl <- makeCluster(workers)
  clusterExport(cl, "prices_sample", envir = environment())
  clusterCall(cl, function() library(exuber))
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
        y <- tryCatch(radf(x$close, lag = exuber_lag, minw = psy_minw(windows[i])), error = function(e) NA)
        if (all(is.na(y))) {
          return(NULL)
        } else {
          stats <- tidy(y)
          bsadf <- data.table::last(augment(y))[, 4:5]
          result <- cbind(symbol = x$symbol[1], date = x$date[length(x$date)], stats, bsadf)
          result$id <- NULL
          colnames(result)[3:ncol(result)] <- paste("exuber", windows[i], exuber_lag, colnames(result)[3:ncol(result)], sep = "_")
          as.data.table(result)
        }
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
  gc()
  stopCluster(cl)
  data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)
  return(data_all_windows)
}
# x  <- rolling_exuber(prices_panel[1:350, .(symbol, date, close)], row_index = c(310, 320), windows = c(200, 300), workers = 4L, exuber_lag = 0L)
