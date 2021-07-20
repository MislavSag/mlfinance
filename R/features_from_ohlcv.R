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
#' @import zoo
#' @import future.apply
#' @import TTR
#' @import roll
#' @import RollingWindow
#' @importFrom stats na.omit
#'
#' @export
features_from_ohlcv <- function(ohlcv, window_sizes = c(5, 22)) {

  # solve No visible binding for global variable
  symbol <- open <- high <- low <- close <- volume <- close_ath <- NULL

  # checks
  testSubset(c("symbol", "open", "high", "low", "close"), colnames(ohlcv))
  assert_double(ohlcv$open, lower = 1e-005)
  assert_double(ohlcv$high, lower = 1e-005)
  assert_double(ohlcv$low, lower = 1e-005)
  assert_double(ohlcv$close, lower = 1e-005)

  # close ATH
  ohlcv[, close_ath := (cummax(close) - close) / cummax(close), by = symbol]

  # returns
  new_cols <- paste0("returns_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) close / shift(close, n = w) - 1), by = symbol]

  # rolling volatility
  new_cols <- paste0("sd_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) roll_sd(close, width = w)), by = symbol]

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
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) as.vector(RollingSkew(close, window = w))), by = symbol]

  # rolling kurtosis
  new_cols <- paste0("kurtosis_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) as.vector(RollingKurt(close, window = w))), by = symbol]

  # rolling TA indicators
  new_cols <- paste0("rsi_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) RSI(close, n = w)), by = symbol]
  # new_cols <- paste0("macd_", window_sizes) # DONT UNDERSTAND THE ERROR
  # ohlcv[, (new_cols) := lapply(window_sizes, function(w) MACD(close, nFast = w, nSlow = 3 * w)[, "macd"]), by = symbol]

  # rolling volume
  new_cols <- paste0("volume_", window_sizes)
  ohlcv[, (new_cols) := lapply(window_sizes, function(w) frollmean(volume / 1000, n = w, na.rm = TRUE)), by = symbol]

  # catch 22 TAKES SOME TIME
  # ISSUES: https://github.com/hendersontrent/Rcatch22/issues/33
  # rc22_all <- list()
  # symbols <- unique(ohlcv$symbol)
  # for (i in 1:length(symbols)) {
  #   print(symbols[i])
  #   sample_ <- ohlcv[symbol == symbols[i]]
  #   rc22 <- list()
  #   for (i in 1:length(feature_list)) {
  #     x <- sample_[, lapply(window_sizes, function(w) frollapply(close, n = w, feature_list[i]))]
  #     colnames(x) <- paste0(feature_list[i], "_", window_sizes)
  #     rc22[[i]] <- x
  #     gc()
  #   }
  #   features_ <- do.call(cbind, rc22)
  #   rc22_all[[i]] <- cbind(sample_[, .(symbol, date)], features_)
  #   gc()
  # }

  # new_cols <- paste0("CO_Embed2_Dist_tau_d_expfit_meandiff_", window_sizes)
  # ohlcv[, (new_cols) := lapply(window_sizes, function(w) {
  #   frollapply(close, n = w, CO_Embed2_Dist_tau_d_expfit_meandiff)}), by = symbol]


}
