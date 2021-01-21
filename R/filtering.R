#' @title cusum_filter
#'
#' @description Advances in Financial Machine Learning, Snippet 2.4, page 39.
#'  The Symmetric Dynamic/Fixed CUSUM Filter.
#'
#' @param price vector of prices
#' @param threshold scalar or numeric vector of same size as price. When the change is larger than the threshold, the function captures it as an even
#' @param return_datetime Should the function return datetime object or index of cusum filter
#'
#' @return vector of cusum events or datetime of cusum events
#'
#' @examples
#' data("spy")
#' cusum_events <- cusum_filter(subset(spy, select = c("index", "close")), 0.001)
#'
#' @export
cusum_filter <- function(price, threshold, return_datetime = TRUE) {

  # check argument types
  price <- two_column_check(price)
  checkmate::assert_number(threshold)
  checkmate::assert_logical(return_datetime)

  # solve No visible binding for global variable
  Datetime <- Value <- `.` <- NULL

  # calculate returns
  returns <- diff(log(price$Value))

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

  # return numeric or datetime
  if (return_datetime) {
    return(price[t_events + 1, .(Datetime)])  # add one because we removed one observation in the begging when calculating returns
  } else {
    return(t_events + 1)
  }
}
