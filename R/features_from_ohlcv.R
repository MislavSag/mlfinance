#' @title Genereta Features from OHLCV
#'
#' @description Function calculates basic features from OHLCV financial data
#'
#' @param ohlcv a data.table object with coluimns: symbol, datetime, open, hogh, low, close, volume
#' @param window_sizes Length of window for calculating rolling versions of the indicators.
#'
#' @return Data.table with new features
#'
#' @import data.table
#' @import future.apply
#' @import TTR
#' @import RollingWindow
#' @import checkmate
#' @importFrom stats na.omit setNames
#' @importFrom roll roll_sd roll_lm roll_quantile
#' @importFrom QuantTools roll_percent_rank sma ema
#'
#' @export

# library(data.table)
# library(httr)
# library(mlfinance)
# library(TTR)
# library(roll)
# library(RollingWindow)
# library(checkmate)
# library(equityData)
# library(PerformanceAnalytics)
# library(QuantTools)
# library(doParallel)
# library(forecast)
# library(runner)
# library(Rcatch22)
# library(quarks)
# library(GAS)
# prices <- get_blob_file("prices.rds", container = "fundamentals", save_file = "D:/fundamental_data/prices.rds", refresh_data_old = 100)
# prices <- prices[open > 0 & high > 0 & low > 0 & close > 0 & adjClose > 0] # remove rows with zero and negative prices
# setorderv(prices, c("symbol", "date"))
# prices[, returns := adjClose / data.table::shift(adjClose) - 1, by = symbol]
# prices <- prices[returns < 1] # remove observations where returns are lower than 100%. TODO:: better outlier detection mechanism
# adjust_cols <- c("open", "high", "low")
# prices[, (adjust_cols) := lapply(.SD, function(x) x * (adjClose / close)), .SDcols = adjust_cols] # adjust open, high and low prices
# prices[, close := adjClose]
# prices <- na.omit(prices[, .(symbol, date, open, high, low, close, volume, vwap, returns)])
# prices_n <- prices[, .N, by = symbol]
# prices_n <- prices_n[which(prices_n$N > 150)]  # remove prices with only 60 or less observations
# prices <- prices[symbol %in% prices_n$symbol]
# ohlcv = prices[, .(symbol, date, open, high, low, close, volume)]
# window_sizes = c(5, 22, 22 * 3, 22 * 6)
# prices_panel <- prices[1:1000, .(symbol, date, close)]

features_from_ohlcv <- function(ohlcv, window_sizes = c(5, 22), quantile_divergence_window = c(50, 100))  {

  # solve No visible binding for global variable
  symbol <- open <- high <- low <- close <- volume <- close_ath <- returns_1 <-
    close_above_sma200 <- ema_above_sma200 <- close_above_vwap_20 <- NULL

  # checks
  testSubset(c("symbol", "open", "high", "low", "close"), colnames(ohlcv))
  assert_double(ohlcv$open, lower = 1e-005)
  assert_double(ohlcv$high, lower = 1e-005)
  assert_double(ohlcv$low, lower = 1e-005)
  assert_double(ohlcv$close, lower = 1e-005)

  # import banchmark
  # spy <- get_daily_prices("SPY", start_date = "1990-01-01", end_date = Sys.Date(), blob_file = "SPY.rds")
  # spy <- setorder(spy, "date")

  # close ATH
  ohlcv[, close_ath := (cummax(close) - close) / cummax(close), by = symbol]

  # returns
  new_cols <- paste0("returns_", c(1, window_sizes))
  ohlcv[, (new_cols) := lapply(c(1, window_sizes), function(w) close / shift(close, n = w) - 1), by = symbol]

  # rolling volatility
  new_cols <- paste0("sd_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) roll::roll_sd(returns_1, width = w)), by = symbol]

  # Close-to-Close Volatility
  new_cols <- paste0("sd_close_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) volatility(close, n = w, calc = "close")), by = symbol]
  new_cols <- paste0("sd_parkinson_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) volatility(cbind(open, high, low, close), n = w, calc = "parkinson")),
        by = symbol]
  new_cols <- paste0("sd_rogers.satchell_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) volatility(cbind(open, high, low, close), n = w, calc = "rogers.satchell")),
        by = symbol]
  new_cols <- paste0("sd_gk.yz_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) volatility(cbind(open, high, low, close), n = w, calc = "gk.yz")),
        by = symbol]
  new_cols <- paste0("sd_yang.zhang_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) volatility(cbind(open, high, low, close), n = w, calc = "yang.zhang")),
        by = symbol]

  # rolling skewness
  new_cols <- paste0("skew_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) as.vector(RollingSkew(returns_1, window = w, na_method = "ignore"))), by = symbol]

  # rolling kurtosis
  new_cols <- paste0("kurtosis_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) as.vector(RollingKurt(returns_1, window = w, na_method = "ignore"))), by = symbol]

  # rolling TA indicators
  new_cols <- paste0("rsi_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) RSI(close, n = w)), by = symbol]
  new_cols <- expand.grid("bbands", c("dn", "mavg", "up", "pctB"), window_sizes)
  new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
  ohlcv <- cbind(ohlcv,
                 setNames(do.call(cbind.data.frame, lapply(window_sizes, function(w) BBands(ohlcv$close, n = w))), new_cols))
  new_cols <- paste0("percent_rank_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) roll_percent_rank(close, n = w)), by = symbol]

  # trading rules
  ohlcv[, close_above_sma200 := as.integer(close > sma(close, n = 200)), by = symbol]
  ohlcv[, ema_above_sma200 := as.integer(ema(close, n = 50) > sma(close, n = 200)), by = symbol]
  ohlcv[, close_above_vwap_20 := as.integer(close > TTR::VWAP(close, volume, n = 20)), by = symbol]

  # rolling volume
  new_cols <- paste0("volume_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) frollmean(volume / 1000, n = w, na.rm = TRUE)), by = symbol]
  new_cols <- paste0("volume_rate_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) volume / shift(volume, n = w) - 1), by = symbol]

  # rolling linear regression model: y = 1 + y_t-1 + e
  new_cols <- paste0("lm_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) roll::roll_lm(log(close), date, width = w))$coefficients[2], by = symbol]
  new_cols <- paste0("r2_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) roll::roll_lm(log(close), date, width = w))$r.squared, by = symbol]

  # rolling sharpe ratio
  new_cols <- paste0("sharpe_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) as.vector(RollingSharpe(returns_1, rep(0, length(close)), window = w, na_method = "ignore"))), by = symbol]

  # rolling quantile substraction
  generate_quantile_divergence <- function(ohlcv, p = 0.99, window_sizes = quantile_divergence_window) {
    q_cols <- paste0("q", p * 100, "_close_", window_sizes)
    ohlcv[, (q_cols) := lapply(window_sizes, function(w) roll::roll_quantile(close, width = w, p = p)), by = symbol]
    new_cols <- paste0("q", p * 100, "_close_divergence_", window_sizes)
    ohlcv[, (new_cols) := lapply(q_cols, function(x) (close - get(x)) / close), by = symbol]
    ohlcv[, (q_cols):=NULL]
    return(ohlcv)
  }
  ohlcv <- generate_quantile_divergence(ohlcv, p = 0.01)
  ohlcv <- generate_quantile_divergence(ohlcv, p = 0.25)
  ohlcv <- generate_quantile_divergence(ohlcv, p = 0.5)
  ohlcv <- generate_quantile_divergence(ohlcv, p = 0.75)
  ohlcv <- generate_quantile_divergence(ohlcv, p = 0.25)
  ohlcv <- generate_quantile_divergence(ohlcv, p = 0.99)

  return(ohlcv)
}
