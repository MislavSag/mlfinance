#' @title add_vertical_barrier
#'
#' @description Compute vertical barriers
#'
#' @param t_events Filtered events (e.g. feom CUSUM filter)
#' @param close_date Poxscit object of close vector or some other price vector
#' @param num_days Number of days into the future
#' @param num_hours Number of hours into the future
#' @param num_minutes Number of minutes into the future
#' @param num_seconds Number of seconds into the future
#'
#' @return data.table with 2 columns: t_events and vertical barrier
#'
#' @importFrom lubridate days hours minutes seconds
#' @import data.table
#'
#' @examples
#' data("spy")
#' cusum_events <- cusum_filter(subset(spy, select = c("index", "close")), 0.001)
#' add_vertical_barrier(cusum_events, spy$index, num_days = 1)
#'
#' @export
add_vertical_barrier <- function(t_events,
                                 close_date,
                                 num_days=1,
                                 num_hours=0,
                                 num_minutes=0,
                                 num_seconds=0
) {

  # check argument types
  t_events <- one_column_check(t_events)
  close_date <- one_column_check(close_date)
  checkmate::assert_number(num_days)
  checkmate::assert_number(num_hours)
  checkmate::assert_number(num_minutes)
  checkmate::assert_number(num_seconds)

  # solve No visible binding for global variable
  nearest <- Datetime <- Value <- NULL

  # calculate timedelta based on inputs
  timedelta <- days(num_days) + hours(num_hours) +
    minutes(num_minutes) + seconds(num_seconds)
  t_events_timdelta <- t_events[[1]] + timedelta

  # merge timedelta and close
  t_events_timdelta <- close_date[, nearest := Datetime][
    data.table(Datetime = t_events_timdelta), on = 'Datetime', roll = -Inf
  ]
  vertical_barrier <- t_events_timdelta$nearest
  vertical_barrier <- data.table(t_events, t1 = vertical_barrier, key = 'Datetime')
  vertical_barrier <- vertical_barrier[!is.na(vertical_barrier$t1)]
  return(vertical_barrier)
}


#' @title apply_pt_sl_on_t1
#'
#' @description Exponential moving average standard deviation
#'
#' @param price xts or data.table with index and price columns
#' @param events Indices that signify "events" (e.g. CUSUM events)
#' @param pt_sl Element 0, indicates the profit taking level; Element 1 is stop loss level
#'
#' @import data.table
#' @importFrom lubridate NA_POSIXct_
#' @import future.apply
#'
#' @return data.table; timestamps of when first barrier was touched
apply_pt_sl_on_t1 <- function(price, events, pt_sl = c(1, 1)) {


  # solve No visible binding for global variable
  cum_returns <- Datetime <- Value <- NULL

  # change to data.table
  if (is.data.frame(price)) {
    price <- as.data.table(price)
  }

  # set profit  taking and stop loss multiple for barriers
  profit_taking_multiple = pt_sl[1]
  stop_loss_multiple = pt_sl[2]

  # set profit taking and stop loss
  if (profit_taking_multiple > 0) {
    profit_taking <- data.table(Datetime = events$t0, Value = profit_taking_multiple * events$trgt)
  } else {
    profit_taking <- data.table(Datetime = events$t0, NA)
  }
  if (stop_loss_multiple > 0) {
    stop_loss <- data.table(Datetime = events$t0, Value = -stop_loss_multiple * events$trgt)
  } else {
    stop_loss <- data.table(Datetime = events$t0, NA)
  }

  # easier but slower OPTIMIZE
  t1_ <- future_vapply(events$t1, function(x) ifelse(is.null(which(x == price$Datetime)),
                                                     NA, which(x == price$Datetime)), integer(1))
  t1_ <- ifelse(is.na(t1_), nrow(price), t1_)
  t0_ <- future_vapply(events$t0, function(x) which(x == price$Datetime), integer(1))
  pt_ <- profit_taking$Value
  sl_ <- stop_loss$Value
  side_ <- events$side
  sl <- c()
  pt <- c()
  pt_sl_ <- future_lapply(seq_along(t0_), function(x) {
    closing_prices <- price[t0_[x]:t1_[x]]
    closing_prices[, cum_returns := Value / data.table::first(Value, 1) - 1 * side_[x]]
    sl_time <- closing_prices[cum_returns < sl_[x], Datetime] # Earliest stop loss date
    sl <- ifelse(is.null(sl_time), NA_POSIXct_, sl_time)
    pt_time <- closing_prices[cum_returns > pt_[x], Datetime] # Earliest profit taking date
    pt <- ifelse(is.null(pt_time), NA_POSIXct_, pt_time)  # nafill in data.table !!!!
    return(cbind(pt = pt, sl = sl))
  })
  pt_sl_ <- do.call(rbind, pt_sl_)
  first_touch_dates = data.table(t0 = events$t0,
                                 t1 = events$t1,
                                 pt = as.POSIXct(pt_sl_[, 1], origin = "1970-01-01"),
                                 sl = as.POSIXct(pt_sl_[, 2], origin = "1970-01-01"))
  first_touch_dates
}


#' @title get_events
#'
#' @description Advances in Financial Machine Learning, Snippet 3.6 page 50.
#'   Getting the Time of the First Touch, with Meta Labels
#'
#' @param price xts or data.table with index and price columns
#' @param t_events These are timestamps that will seed every triple barrier (e. g. CUSUM events)
#' @param pt_sl vector; Element 0, indicates the profit taking level; Element 1 is stop loss level.
#'   A non-negative float that sets the width of the two barriers.
#' @param target of values that are used to determine the width of the barrier.
#' @param min_ret he minimum target return required for running a triple barrier search.
#' @param vertical_barrier_times Verticla barrier timestamp
#' @param side_prediction Side of the bet (long/short) as decided by the primary model
#'
#' @return Events
#'  -events t0 is event's starttime
#'  -events t1 is event's endtime
#'  -events trgt is event's target
#'  -events side (optional) implies the algo's position side
#'  -events pt is profit taking multiple
#'  -events sl  is stop loss multiple
#'
#' @import data.table
#' @import checkmate
#'
#' @examples
#' data("spy")
#' close <- subset(spy, select = c("index", "close"))
#' daily_vol <- daily_volatility(close)
#' cusum_events <- cusum_filter(close, 0.001)
#' vertical_barriers <- add_vertical_barrier(cusum_events, spy$index, num_days = 1)
#' events <- get_events(price = close,
#'                      t_events = cusum_events,
#'                      pt_sl = c(1, 2),
#'                      target = daily_vol,
#'                      min_ret = 0.005,
#'                      vertical_barrier_times=vertical_barriers,
#'                      side_prediction=NA)
#'
#' @export
get_events <- function(price,
                       t_events,
                       pt_sl,
                       target,
                       min_ret,
                       vertical_barrier_times=NA,
                       side_prediction=NA) {

  # check argument types
  price <- two_column_check(price)
  t_events <- one_column_check(t_events)
  checkmate::assert_numeric(pt_sl, len = 2L, lower = 0, upper = 1000,
                            finite = TRUE, any.missing = FALSE, all.missing = FALSE)
  target <- two_column_check(target)
  checkmate::assert_number(min_ret)
  checkmate::assert_class(vertical_barrier_times, "data.table", null.ok = TRUE)

  # solve No visible binding for global variable
  t1 <- side <- trgt <- t1_min <- pt <- sl <- Datetime <- Value <- `.` <- NULL

  # 1) Get target
  target = target[Datetime %in% t_events$Datetime]
  target = target[Value > min_ret]  # min_ret

  # 2) Get vertical barrier (max holding period)
  if (test_scalar_na(vertical_barrier_times)) {
    vertical_barrier_times <- data.table(t_events, t1 = rep(NA, length(t_events)))
  }

  # 3) Form events object, apply stop loss on vertical barrier
  if (test_scalar_na(side_prediction)) {
    side_ <- data.table(Datetime =target$Datetime, side = rep(1, nrow(target)))
    # conames(side_) <- c('Datetime', 'side')
  } else {
    side_ <- side_prediction
  }

  # Create a new df with [v_barrier, target, side] and drop rows that are NA in target
  events <- vertical_barrier_times[target, on = 'Datetime'][side_, on = 'Datetime']
  events <- events[, .(Datetime, t1, Value, side)]
  colnames(events) <- c('t0', 't1', 'trgt', 'side')
  events <- events[!is.na(trgt)]

  # apply Triple Barrier
  first_touch_dates <- apply_pt_sl_on_t1(price, events, pt_sl = pt_sl)

  # get minimum date from every row of first_touch_dates (not including first column)
  first_touch_dates[, t1_min := pmin(t1, pt, sl, na.rm = TRUE)]
  events[, t1 := first_touch_dates$t1_min]

  # dropsides if side_prediction NA
  if (test_scalar_na(side_prediction)) events[, side := NULL]

  # Add profit taking and stop loss multiples for vertical barrier calculations
  events$pt <- pt_sl[1]
  events$sl <- pt_sl[2]
  events
}


#' @title get_bins
#'
#' @description Advances in Financial Machine Learning, Snippet 3.7, page 51.
#'   Labeling for Side & Size with Meta Labels
#'
#' @param triple_barrier_events data.table or data.frame from get_events function
#' @param price xts or data.table with index and price columns
#'
#' @return Events
#'  -events Datetime is event's start time
#'  -events ret is event's target
#'  -events trgt is event's target
#'  -events bin label; bin in (-1,1) or bin in (0,1)if metalabel
#'
#' @import data.table
#' @import checkmate
#'
#' @examples
#' data("spy")
#' close <- subset(spy, select = c("index", "close"))
#' daily_vol <- daily_volatility(close)
#' cusum_events <- cusum_filter(close, 0.001)
#' vertical_barriers <- add_vertical_barrier(cusum_events, spy$index, num_days = 1)
#' events <- get_events(price = close,
#'                      t_events = cusum_events,
#'                      pt_sl = c(1, 2),
#'                      target = daily_vol,
#'                      min_ret = 0.005,
#'                      vertical_barrier_times=vertical_barriers,
#'                      side_prediction=NA)
#' labels <- get_bins(events, close)
#'
#' @export
# data("spy")
# close <- subset(spy, select = c("index", "close"))
# daily_vol <- daily_volatility(close)
# cusum_events <- cusum_filter(close, 0.001)
# vertical_barriers <- add_vertical_barrier(cusum_events, spy$index, num_days = 1)
# events <- get_events(price = close,
#                     t_events = cusum_events,
#                       pt_sl = c(1, 2),
#                       target = daily_vol,
#                       min_ret = 0.005,
#                       vertical_barrier_times=vertical_barriers,
#                       side_prediction=NA)
# labels <- get_bins(events, close)

get_bins <- function(triple_barrier_events, price) {

  # check argument types
  price <- two_column_check(price)
  checkmate::assert_character(colnames(triple_barrier_events),
                              pattern = 't0|t1|trgt|side|pt|sl',
                              min.len = 5, max.len = 6)

  # solve No visible binding for global variable
  t1 <- t0 <- bin <- pt <- sl <- Datetime <- Value <- `.` <- NULL

  # 1) Calculate returns
  events_ <- triple_barrier_events[!is.na(t1)]
  returns <- c()
  for (i in seq_along(events_$t0)) {
    x <- price[Datetime %between% list(events_$t0[i], events_$t1[i]), Value]
    x <- log(x[length(x)]) - log(x[1])
    returns[i] <- x
  }

  # 2) Create out DataFrame
  out_df <- data.table(Datetime = events_$t0, ret = returns, trgt = events_$trgt)

  # Meta labeling: Events that were correct will have pos returns
  if ('side' %in% colnames(events_)) {
    out_df$ret <- out_df$ret * events_$side
  }

  # Added code: label 0 when vertical barrier reached
  store = c()
  for (i in seq_along(out_df$Datetime)) {
    ret <- out_df$ret[i]
    target <- out_df$trgt[i]
    datetime <- out_df$Datetime[i]

    pt_level_reached = ret > log(1 + target) * events_[t0 == datetime, pt]
    sl_level_reached = ret < -log(1 + target) * events_[t0 == datetime, sl]

    if (ret > 0.0 & pt_level_reached) {
      # Top barrier reached
      store <- c(store, 1)
    } else if (ret < 0.0 & sl_level_reached) {
      store <- c(store, -1)
    } else {
      store <- c(store, 0)
    }
  }
  out_df$bin <- store

  # Meta labeling: label incorrect events with a 0
  if ('side' %in% colnames(events_)) {
    out_df[out_df$ret < 0, bin] <- 0
  }

  # Transform the log returns back to normal returns.
  out_df$ret <- exp(out_df$ret) - 1

  # Add the side to the output. This is useful for when a meta label model must be fit
  if ('side' %in% colnames(triple_barrier_events)) {
    out_df$side <- triple_barrier_events$side
  }
  return(out_df)
}


#' @title drop_labels
#'
#' @description Advances in Financial Machine Learning, Snippet 3.8 page 54.
#'   This function recursively eliminates rare observations.
#'
#' @param events Events with labels
#' @param min_pct A fraction used to decide if the observation occurs less than that fraction.
#'
#' @import data.table
#' @import checkmate
#'
#' @examples
#' data("spy")
#' close <- subset(spy, select = c("index", "close"))
#' daily_vol <- daily_volatility(close)
#' cusum_events <- cusum_filter(close, 0.001)
#' vertical_barriers <- add_vertical_barrier(cusum_events, spy$index, num_days = 1)
#' events <- get_events(price = close,
#'                      t_events = cusum_events,
#'                      pt_sl = c(1, 2),
#'                      target = daily_vol,
#'                      min_ret = 0.005,
#'                      vertical_barrier_times=vertical_barriers,
#'                      side_prediction=NA)
#' labels <- get_bins(events, close)
#' labels_red <- drop_labels(labels, 0.05)
#'
#' @export
drop_labels <- function(events, min_pct = 0.05) {

  # check argument types
  checkmate::assert_character(colnames(events),
                              pattern = 'Datetime|ret|trgt|bin',
                              min.len = 4, max.len = 6)
  checkmate::assert_number(min_pct)

  # apply bin reduction
  while (TRUE) {
    df0 <- prop.table(table(events$bin))
    if (min(df0) > min_pct | length(df0) < 3) {
      break()
    }
    print(paste0('dropped label: ', names(df0)[which.min(df0)], ' ', min(df0)))
    events <- events[events$bin != as.integer(names(df0)[which.min(df0)])]
  }
  return(events)
}
