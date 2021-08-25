#' @title Rolling GAS
#'
#' @description Function calculates GAS risk values from GAS package on rolling window.
#'
#' @param prices_panel a data.table object with columns: symbol, datetime, close.
#' @param row_index row index for which to calculate radf values.
#' @param windows Length of window for calculating forecasts on rolling window.
#' @param workers Number of workers for parallel processing
#' @param gas_dist Dist parametere in UniGASSpec fucntionUniGASSpec.
#' @param gas_scaling Scaling parametere in UniGASSpec fucntionUniGASSpec.
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import checkmate
#' @import doParallel
#' @import runner
#' @import GAS
#'
#' @export
rolling_gas <- function(prices_panel, row_index = 1:nrow(prices_panel), windows = c(200), workers = 4L, gas_dist = "sstd", gas_scaling = "Identity") {

  # solve No visible binding for global variable
  symbol <- close <- NULL

  # checks
  testSubset(c("symbol", "close"), colnames(prices_panel))
  assert_double(prices_panel$close, lower = 1e-005)

  # prepare data
  prices_panel[, returns := close / shift(close) - 1]
  prices_sample <- prices_panel[, .(symbol, date, returns)]

  # start cluster
  cl <- makeCluster(workers)
  clusterExport(cl, c("prices_sample", "get_series_statistics"), envir = environment())
  clusterCall(cl, function() library(GAS))
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
        GASSpec <- UniGASSpec(Dist = gas_dist, ScalingType = gas_scaling,
                              GASPar = list(location = TRUE, scale = TRUE, shape = TRUE, skewness = TRUE))
        Fit <- tryCatch(UniGASFit(GASSpec, na.omit(x$returns)), error = function(e) NA)
        if (all(is.na(Fit))) {
          return(NA)
        } else {
          y <- UniGASFor(Fit, H = 62, ReturnDraws = TRUE)

          # define statistics
          q <- as.data.table(quantile(y, p = c(0.01, 0.05)))
          q <- get_series_statistics(q, "var")
          es <- as.data.table(ES(y, p = c(0.01, 0.05)))
          es <- get_series_statistics(es, "es")
          moments <- as.data.table(getMoments(y))
          moments <- get_series_statistics(moments, "moments")
          f <- as.data.table(getForecast(y))
          f <- get_series_statistics(f, "f")
          results <- cbind(symbol = x$symbol[1], date = x$date[length(x$date)], q, es, moments, f)
          colnames(results)[3:ncol(results)] <- paste(colnames(results)[3:ncol(results)], windows[i], sep = "_")
          return(results)
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
  stopCluster(cl)
  data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)
  return(data_all_windows)
}
# start_time <- Sys.time()
# x  <- rolling_gas(prices_panel[1:350, .(symbol, date, close)], row_index = c(300:400), windows = c(200, 300), workers = 20L, gas_dist = "sstd", gas_scaling = "Identity")
# end_time <- Sys.time()
# end_time - start_time

get_series_statistics <- function(df, colname_prefix = "var") {
  stats <- lapply(df, function(x) {
    var_1 <- x[1]
    var_subsample <- mean(x[1:(length(x)/2)], na.rm = TRUE)
    var_all <- mean(x, na.rm = TRUE)
    var_std <- sd(x, na.rm = TRUE)
    list(var_1 = var_1, var_subsample = var_subsample, var_all = var_all, var_std = var_std)
  })
  stats <- melt(rbindlist(stats, idcol = "id"), id.vars = "id")
  stats[, col_name := paste(variable, gsub("\\.", "_", id), sep = "_")]
  stats <- transpose(stats[, .(col_name, value)], make.names = TRUE)
  colnames(stats) <- gsub("var", colname_prefix, colnames(stats))
  return(stats)
}

