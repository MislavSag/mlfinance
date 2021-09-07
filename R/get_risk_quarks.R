#' Get Complete Earnings Calendar from yahoo
#'
#' @description Get complete earnings calendar from yahoo.
#'
#' @param prices_panel a data.table object with coluimns: symbol, datetime, open, hogh, low, close, volume
#' @param p Quant
#' @param end_date End date
#' @param blob_file Blob file
#'
#' @import httr
#' @import data.table
#' @import AzureStor
#' @import rvest
#' @import stringr
#' @import parallel
#' @import quarks
#' @import runner
#' @importFrom parallel makeCluster
#'
#' @return Data frame with yahoo earnings calnedar data
#'
#' @export
# model = c("EWMA")
# method = c("age", "vwhs", "fhs")
# ##### DOESNT WORK YET ##################
# x <- prices_sample[1:1000]
# y <- rollcast(na.omit(x$returns)[1:202],
#               p = params_$p,
#               model = params_$model,
#               method = params_$method,
#               nout = 2,
#               nwin = params_$win_size
# )
# na.omit(x$returns)[202]
#
# get_risk_quarks <- function(prices_panel, row_index = 1:nrow(prices_panel), p = c(0.975), win_size = 200, model = c("EWMA", "GARCH"),
#                             method = c("plain", "age", "vwhs", "fhs"), forecast_length = 100, workers = 4L) {
#
#   # definr parametres set
#   params <- expand.grid(p, model, method, win_size, forecast_length, stringsAsFactors = FALSE)
#   colnames(params) <- c("p", "model", "method", "win_size", "forecast_length")
#   params <- params[!(params$model == 'GARCH' & params$method == "fhs"), ]
#
#   # solve No visible binding for global variable
#   symbol <- open <- high <- low <- close <- volume <- prices <- returns_1 <- NULL
#
#   # checks
#   testSubset(c("symbol", "close"), colnames(prices_panel))
#   assert_double(prices_panel$close, lower = 1e-005)
#
#   # prepare data
#   prices_panel[, returns := close / shift(close) - 1]
#   prices_sample <- prices_panel[, .(symbol, date, returns)]
#
#   # rolling arima forecasts
#   data_list <- list()
#   for (i in 1:length(params)) {
#     params_ <- params[i, ]
#
#     # start cluster
#     cl <- makeCluster(workers)
#     clusterExport(cl, c("prices_sample", "params_", "i"), envir = environment())
#     clusterCall(cl, function() library(quarks))
#     clusterCall(cl, function() library(data.table))
#
#     rolling_data <- runner(
#       x = prices_sample,
#       f = function(x) {
#
#         # check if there is enough data
#         print(length(na.omit(x$returns)))
#         print(params_$win_size)
#         if (length(unique(x$symbol)) > 1) {
#           print(paste0("not enough data for symbol ", x$symbol[1]))
#           return(NA)
#         } else if (length(na.omit(x$returns)) < params_$win_size) {
#           print(paste0("Not enough data for window"))
#           return(NA)
#         }
#
#         # calculate arima forecasts
#         if (params_$method == "fhs") {
#           y <- rollcast(na.omit(x$returns),
#                         p = params_$p,
#                         model = params_$model,
#                         method = params_$method,
#                         nout = params_$forecast_length,
#                         nwin = params_$win_size,
#                         nboot = 500
#           )
#         } else {
#           y <- rollcast(na.omit(x$returns),
#                         p = params_$p,
#                         model = params_$model,
#                         method = params_$method,
#                         nout = params_$forecast_length,
#                         nwin = params_$win_size
#           )
#         }
#         VaR <- as.data.table(get_series_statistics(y$VaR))
#         ES <- as.data.table(get_series_statistics(y$ES))
#         colnames(ES) <- gsub("var_", "es_", colnames(ES))
#         result <- cbind.data.frame(VaR, ES)
#         colnames(result) <- paste0(colnames(result), paste(params_$p * 1000, paste0(params_[2:ncol(params_)], collapse = "_"), sep = "_"))
#         return(result)
#       },
#       k = params_$win_size[i] + 120,
#       at = row_index,
#       na_pad = TRUE,
#       simplify = FALSE
#       # cl = cl
#     )
#     stopCluster()
#     gc()
#     data_list[[i]] <- rbindlist(rolling_data[lengths(rolling_data) > 1L])
#   }
#   stopCluster(cl)
#   data_all_windows <- rbindlist(data_list)
#   return(data_all_windows)
# }
#
#
# # get_ris_quarks(prices, p = 0.975, win_size = 200, model = "EWMA", method = c("age", "vwhs", "fhs"))
#
# get_series_statistics <- function(series) {
#   var_1 <- series[1]
#   var_day <- mean(series[1:8], na.rm = TRUE)
#   var_week <- mean(series[1:40], na.rm = TRUE)
#   var_month <- mean(series, na.rm = TRUE)
#   var_std <- sd(series, na.rm = TRUE)
#   return(list(var_1 = var_1, var_day = var_day, var_week = var_week, var_month = var_month, var_std = var_std))
# }
#
#
