#' @title cusum_filter
#'
#' @description Advances in Financial Machine Learning, Snippet 2.4, page 39.
#'  The Symmetric Dynamic/Fixed CUSUM Filter.
#'
#' @param price vector of prices
#' @param threshold scalar or numeric vector of same size as price. When the change is larger than the threshold, the function captures it as an even
#'
#' @return vector of cusum events
#'
#' @examples
#' data("spy")
#' cusum_events <- cusum_filter(spy$close, 0.001)
#' cusum_events <- spy$index[cusum_events]
#'
#' @export

cusum_filter <- function(price, threshold) {

  # calculate returns
  returns <- diff(log(price))

  # threshold to vector if on element
  if (length(threshold) == 1) {
    threshold <- rep(threshold, length(returns))
  } else {
    threshold <- threshold[2:length(returns)]
  }

  # init vars for loop
  s_pos <- 0
  s_neg <- 0
  t_events = c()

  # clacluate cusum events0
  for (i in seq_along(returns)) {
    s_pos <- max(0, s_pos + returns[i])
    s_neg <- min(0, s_neg + returns[i])
    if (s_neg < -threshold[i]) {
      s_neg <- 0
      t_events <- c(t_events, i)
    }
    if (s_pos > threshold[i]) {
      s_pos <- 0
      t_events <- c(t_events, i)
    }
  }
  # add one because we removed one observation in the begging when calculating returns
  t_events + 1
}
