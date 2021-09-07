#' @title Rolling catch 22
#'
#' @description Function calculates catch 22 features on rolling window.
#'
#' @param prices_panel a data.table object with coluimns: symbol, datetime, open, hogh, low, close, volume
#' @param row_index row index for which to calculate radf values.
#' @param windows Length of window for calculating forecasts on rolling window.
#' @param workers Number of workers for parallel processing
#' @param features_set Look at teft package
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import checkmate
#' @import forecast
#' @import doParallel
#' @import runner
#' @import theft
#' @import reticulate
#' @importFrom parallel makeCluster clusterExport clusterCall stopCluster
#'
#' @export
rolling_theft <- function(prices_panel, row_index = 1:nrow(prices_panel), windows = c(200), workers = 8L,
                          features_set = c("catch22", "feasts", "tsfeatures", "kats", "tsfresh", "tsfel")) {

  # set python path
  reticulate::use_python("C:/ProgramData/Anaconda3/envs/theft/python.exe", required = TRUE)

  # solve No visible binding for global variable
  symbol = open = high = low = close = volume = prices = returns_1 =
    var_names = method = values = `.` = NULL

  # checks
  testSubset(c("symbol", "close"), colnames(prices_panel))
  assert_double(prices_panel$close, lower = 1e-005)

  # prepare data
  prices_sample <- prices_panel[, .(symbol, date, close)]

  # start cluster
  cl <- makeCluster(workers)
  clusterExport(cl, "prices_sample", envir = environment())
  clusterCall(cl, function() library(theft))
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

        # calculate features
        y <- as.data.table(calculate_features(x, "symbol", "date", "close", feature_set = features_set, tsfresh_cleanup = TRUE))
        y[, var_names := paste(method, names, windows[i], sep = "_")]
        y <- transpose(y[, .(var_names, values)], make.names = TRUE)
        results <- data.table(symbol = x$symbol[1], date = x$date[length(x$date)], y)
        results
      },
      k = windows[i],
      at = row_index,
      na_pad = TRUE,
      simplify = FALSE,
      cl = cl
    )
    gc()
    data_list[[i]] <- rbindlist(rolling_data[lengths(rolling_data) > 1L], fill = TRUE)
  }
  stopCluster(cl)
  data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)

  # # remove same columns
  # features <- as.data.frame(data_all_windows)
  # features <- features[!duplicated(lapply(features, summary, digits = 1)), 3:ncol(features)]
  # colnames(features) <- tolower(gsub("\\.| |-", "_", colnames(features)))

  return(data_all_windows)
}
