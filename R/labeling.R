#' @title daily_volatility
#'


# # utils
# f <- function(y, alpha) {
#   m <- length(y)
#   weights <- (1 - alpha)^((m - 1):0)
#   ewma <- sum(weights * y) / sum(weights)
#   bias <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
#   ewmsd <- sqrt(bias * sum(weights * (y - ewma)^2) / sum(weights))
#   ewmsd
# }
#
#
#
# # add vertical bar
# add_vertical_barrier <- function(t_events,
#                                  close_date,
#                                  num_days=1,
#                                  num_hours=0,
#                                  num_minutes=0,
#                                  num_seconds=0
# ) {
#   timedelta <- days(num_days) + hours(num_hours) + minutes(num_minutes) + seconds(num_seconds)
#   t_events_timdelta <- t_events[[1]] + timedelta
#   t_events_timdelta <- as.data.table(close_date)[,nearest := close_date][
#     as.data.table(t_events_timdelta), on = 'x', roll = -Inf
#   ]
#   vertical_barrier <- t_events_timdelta$nearest
#   vertical_barrier <- data.table(index = t_events$index, t1 = vertical_barrier, key = 'index')
#   vertical_barrier <- vertical_barrier[!is.na(vertical_barrier$t1)]
#   return(vertical_barrier)
# }
#
#
# apply_pt_sl_on_t1 <- function(close, events, pt_sl = c(1, 1)) {
#   profit_taking_multiple = pt_sl[1]
#   stop_loss_multiple = pt_sl[2]
#
#   if (profit_taking_multiple > 0) {
#     profit_taking <- data.table(index = events$t0, profit_taking_multiple * events$trgt)
#   } else {
#     profit_taking <- data.table(index = events$t0, NA)
#   }
#   if (stop_loss_multiple > 0) {
#     stop_loss <- data.table(index = events$t0, -stop_loss_multiple * events$trgt)
#   } else {
#     stop_loss <- data.table(index = events$t0, NA)
#   }
#
#
#   # easier but slower OPTIMIZE
#   t1_ <- events$t1
#   t1_[is.na(loop_index)] <- tail(close$index, 1)
#   t0_ <- events$t0
#   pt_ <- profit_taking$V2
#   sl_ <- stop_loss$V2
#   side_ <- events$side
#   sl <- c()
#   pt <- c()
#   for (i in seq_along(t0_)) {
#     closing_prices <- close[index %between% c(t0_[i], t1_[i])]
#     closing_prices[, cum_returns := close / head(close, 1) - 1 * side_[i]]
#     sl_time <- closing_prices[cum_returns < sl_[i], index] # Earliest stop loss date
#     sl <- c(sl, ifelse(is.null(sl_time), NA_POSIXct_, sl_time))
#     pt_time <- closing_prices[cum_returns > pt_[i], index] # Earliest profit taking date
#     pt <- c(pt, ifelse(is.null(pt_time), NA_POSIXct_, pt_time))  # nafill in data.table !!!!
#   }
#   first_touch_dates = data.table(t0 = events$t0,
#                                  t1 = events$t1,
#                                  pt = as.POSIXct(pt, origin = "1970-01-01"),
#                                  sl = as.POSIXct(sl, origin = "1970-01-01"))
#   first_touch_dates
# }
#
#
#
# get_events <- function(close,
#                        t_events,
#                        pt_sl,
#                        target,
#                        min_ret,
#                        # num_threads,
#                        vertical_barrier_times=FALSE,
#                        side_prediction=NA) {
#
#   # CHECKED SAME AS DAILY VOL< EVEN BETTER BECAUSE FIRST OBS REMOVED
#   # 1) Get target
#   target = target[index %in% t_events$index]
#   target = target[daily_vol > min_ret]  # min_ret
#
#   # 2) Get vertical barrier (max holding period)
#   if (isFALSE(vertical_barrier_times)) {
#     vertical_barrier_times <- data.table(index = t_events, t1 = rep(NA, length(t_events)))
#   }
#
#   # 3) Form events object, apply stop loss on vertical barrier
#   if (is.na(side_prediction)) {
#     side_ <- data.table(index = target$index, side = rep(1, nrow(target)))
#   } else {
#     side_ <- side_prediction
#   }
#
#   # CHECKED GOOD
#   # Create a new df with [v_barrier, target, side] and drop rows that are NA in target
#   events <- vertical_barrier_times[target, on = 'index'][side_, on = 'index']
#   events <- events[, .(index, t1, daily_vol, side)]
#   colnames(events) <- c('t0', 't1', 'trgt', 'side')
#   events <- events[!is.na(trgt)]
#
#   # apply Triple Barrier
#   first_touch_dates <- apply_pt_sl_on_t1(close, events, pt_sl = pt_sl)
#
#   # get minimum date from every row of first_touch_dates (not including first column)
#   first_touch_dates[, t1_min := pmin(t1, pt, sl, na.rm = TRUE)]
#   events[, t1 := first_touch_dates$t1_min]
#
#   # dropsides if side_prediction NA
#   if (is.na(side_prediction)) events[, side := NULL]
#
#   # Add profit taking and stop loss multiples for vertical barrier calculations
#   events$pt <- pt_sl[1]
#   events$sl <- pt_sl[2]
#   events
# }
#
#
# get_bins <- function(triple_barrier_events, close) {
#   # 1) Calculate returns
#   events_ <- triple_barrier_events[!is.na(t1)]
#   returns <- c()
#   for (i in seq_along(events_$t0)) {
#     x <- close[index %between% list(events_$t0[i], events_$t1[i]), close]
#     x <- log(x[length(x)]) - log(x[1])
#     returns[i] <- x
#   }
#
#   # 2) Create out DataFrame
#   out_df <- data.table(index = events_$t0, ret = returns, trgt = events_$trgt)
#
#   # Meta labeling: Events that were correct will have pos returns
#   if ('side' %in% colnames(events_)) {
#     out_df$ret <- out_df$ret - events_['side']
#   }
#
#   # Added code: label 0 when vertical barrier reached
#   store = c()
#   for (i in seq_along(out_df$index)) {
#     ret <- out_df$ret[i]
#     target <- out_df$trgt[i]
#     datetime <- out_df$index[i]
#
#     pt_level_reached = ret > log(1 + target) * events_[t0 == datetime, pt]
#     sl_level_reached = ret < -log(1 + target) * events_[t0 == datetime, sl]
#
#     if (ret > 0.0 & pt_level_reached) {
#       # Top barrier reached
#       store <- c(store, 1)
#     } else if (ret < 0.0 & sl_level_reached) {
#       store <- c(store, -1)
#     } else {
#       store <- c(store, 0)
#     }
#   }
#   out_df$bin <- store
#
#   # Meta labeling: label incorrect events with a 0
#   if ('side' %in% colnames(events_)) {
#     out_df[out_df$ret < 0, bin] <- 0
#   }
#
#   # Transform the log returns back to normal returns.
#   out_df$ret <- exp(out_df$ret) - 1
#
#   # Add the side to the output. This is useful for when a meta label model must be fit
#   if ('side' %in% colnames(triple_barrier_events)) {
#     out_df$side <- triple_barrier_events$side
#   }
#   return(out_df)
# }
#
#
#
# drop_lables <- function(events, min_pct = 0.05) {
#   while (TRUE) {
#     df0 <- prop.table(table(events$bin))
#     if (min(df0) > min_pct | length(df0) < 3) {
#       break()
#     }
#     print(paste0('dropped label: ', names(df0)[which.min(df0)], ' ', min(df0)))
#     events <- events[events$bin != as.integer(names(df0)[which.min(df0)])]
#   }
#   return(events)
# }
