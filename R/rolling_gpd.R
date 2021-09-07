#' @title Rolling GPD
#'
#' @description Function calculates GPD risk measures on rolling window from ptsuite pakcage.
#'
#' @param prices_panel a data.table object with coluimns: symbol, datetime, open, hogh, low, close, volume
#' @param row_index row index for which to calculate radf values.
#' @param windows Length of window for calculating forecasts on rolling window.
#' @param workers Number of workers for parallel processing
#' @param threshold threshold parameter for returns. Returns below/above this threshold will be input.
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import checkmate
#' @import ptsuite
#' @import doParallel
#' @import runner
#' @importFrom parallel makeCluster clusterExport clusterCall stopCluster
#'
#' @export
rolling_gpd <- function(prices_panel,
                        row_index = 1:nrow(prices_panel),
                        windows = c(200),
                        workers = 4L,
                        threshold = 0.03) {

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
  clusterExport(cl, c("prices_sample", "estimate_gpd"), envir = environment())
  clusterCall(cl, function() library(ptsuite))
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

        # returns
        r <- na.omit(x$returns)

        # pareto left tail
        x_left <- r[r < -threshold] * -1
        risks_left <- estimate_gpd(x_left, hill_threshold = 0.02, suffix = '_left')

        # pareto test right tail
        x_right <- r[r > threshold]
        risks_right <- estimate_gpd(x_right, hill_threshold = 0.02)

        # estimate shape parameters
        pareto_tests <- cbind(risks_left, risks_right)
        results <- data.table(symbol = x$symbol[1], date = x$date[length(x$date)], pareto_tests)
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
    data_list[[i]] <- rbindlist(forecasts[lengths(forecasts) > 1L], fill = TRUE)
  }
  stopCluster(cl)
  data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)
  return(data_all_windows)
}

# test <- rolling_gpd(prices_panel,
#                     row_index = 1:nrow(prices_panel),
#                     windows = c(200),
#                     workers = 4L,
#                     threshold = 0.03)

#' @title Rolling GPD Help Function
#'
#' @description Help function for calculating GPD features.
#'
#' @param x vector of (filtered) returns
#' @param hill_threshold look at ptsuite package
#' @param suffix suffix for column names
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import ptsuite
#'
#' @export
estimate_gpd <- function(x, hill_threshold = 0.02, suffix = '_right') {
  columns_names <- c(
    'ptest',
    'hill_shape',
    'scales_geometric_percentiles_method',
    'scales_least_squares',
    'scales_method_of_moments',
    'scales_modified_percentiles_method',
    'scales_weighted_least_squares',
    'shapes_geometric_percentiles_method',
    'shapes_least_squares',
    'shapes_method_of_moments',
    'shapes_modified_percentiles_method',
    'shapes_weighted_least_squares'
  )
  columns_names <- paste0(columns_names, suffix)
  if (length(x) == 0) {
    risks <- data.table(t(rep(0, length(columns_names))))
    setnames(risks, colnames(risks), columns_names)
  } else {
    ptest <- pareto_test(x)$`p-value`
    estimates <- as.data.table(generate_all_estimates(x))
    shapes <- data.table::dcast(estimates[, 1:2], . ~ `Method.of.Estimation`, value.var = 'Shape.Parameter')
    shapes <- shapes[, 2:ncol(shapes)]
    colnames(shapes) <- paste0('shapes_', gsub(" ", "", tolower(colnames(shapes))))
    scales <- data.table::dcast(estimates[, c(1, 3)], . ~ `Method.of.Estimation`, value.var = 'Scale.Parameter')
    scales <- scales[, 2:ncol(scales)]
    colnames(scales) <- paste0('scales_', gsub(" ", "", tolower(colnames(scales))))
    hill_estimate <- alpha_hills(x, hill_threshold, FALSE)
    hill_shape <- hill_estimate$shape
    risks <- as.data.table(data.frame(as.list(c(scales, shapes))))
    risks <- cbind(ptest, hill_shape, risks)
    colnames(risks) <- paste0(colnames(risks), suffix)
  }
  return(risks)
}
