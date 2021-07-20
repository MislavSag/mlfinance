#' @title Abnormal returns labeling
#'
#' @description Define labels based on abnormal returns extracted from firm returns and events.
#'
#' @param events a data.frame of two columns with event dates (colname: “when”) and column names of the ‘response’ series from ‘firm.returns’ (colname “name”).
#' @param firm_returns zoo matrix of ‘outcome’ or ‘response’ series
#' @param market_returns Returns of SPY or some similiar asset.
#' @param event_horizon an ‘integer’ of length 1 that specifies a symmetric event window around the event time as specified in the index of “firm.returns”.
#'
#' @return Two data frames. First contains labels and abnormal returns and second contains whole returns path.
#'
#' @import data.table
#' @import zoo
#' @import eventstudies
#' @import future.apply
#' @importFrom stats na.omit
#'
#' @export
labeling_abnormal_returns <- function(events,
                                      firm_returns,
                                      market_returns,
                                      event_horizon = 30) {


  # solve No visible binding for global variable
  name <- NULL

  # checks
  min_market_return_date <- min(zoo::index(market_returns)) + 5
  firm_returns <- firm_returns[paste0(min_market_return_date, "/")]

  eventstudy_results <- future_lapply(unique(events$name), function(s) {
    print(s)

    # checks
    if (!(s %in% colnames(firm_returns))) {
      print(paste0("There is no ", s, " symbol in prices"))
      return(NULL)
    }

    # define events and returns subsample
    events_ <- as.data.frame(events[name %in% s])
    prices_ <- na.omit(firm_returns[, s])

    # event studies
    es_results <- lapply(1:nrow(events_), function(i) {
      print(i)
      subsample <- as.zoo(prices_[paste0(events_$when[i] - (event_horizon + 100), "/", events_$when[i] + event_horizon + 100)])
      if (length(subsample) == 0 | Inf %in% subsample) {
        return(NULL)
      } else {
        es <- eventstudy(firm.returns = subsample,
                         event.list = events_[i, ],
                         event.window = event_horizon,
                         type = "marketModel",
                         to.remap = FALSE,
                         inference = FALSE,
                         model.args = list(
                           market.returns = as.zoo(market_returns)
                         )
        )
      }
    })

    # outcomes
    outcomes <- lapply(es_results, function(x) x$outcomes)
    outcomes[unlist(lapply(outcomes,is.null))] <- "nodata"
    abnormal_returns <- lapply(es_results, function(x) x$result)
    abnormal_returns_test <- lapply(abnormal_returns, function(x) x[(length(x)-event_horizon-1):length(x)])
    abnormal_returns_test_last <- lapply(abnormal_returns_test, function(x) prod(1 + x) - 1)

    # labels
    labels <- cbind.data.frame(symbol = s,
                               datetime = events_$when,
                               outcomes = unlist(outcomes),
                               abnormal_returns_test_last = unlist(abnormal_returns_test_last))

    # returns
    abnormal_returns_dt <- lapply(abnormal_returns, as.data.table)
    abnormal_returns_dt <- lapply(abnormal_returns_dt, t)
    abnormal_returns_dt <- lapply(abnormal_returns_dt, function(x) {

      if (length(x) == 0) {
        x <- t(as.matrix(rep(NA, 60)))
      }
      x
    })
    abnormal_returns_dt <- as.data.table(do.call(rbind, abnormal_returns_dt))
    abnormal_returns_dt <- cbind(symbol = s,
                                 datetime = events_$when,
                                 outcomes = unlist(outcomes), abnormal_returns_dt)

    return(list(labels = labels, abnormal_returns_dt = abnormal_returns_dt))
  })
  eventstudy_labels_dt <- rbindlist(lapply(eventstudy_results, `[[`, "labels"))
  eventstudy_returns_dt <- rbindlist(lapply(eventstudy_results, `[[`, "abnormal_returns_dt"))

  return(list(eventstudy_labels_dt = eventstudy_labels_dt,
              eventstudy_returns_dt = eventstudy_returns_dt))
}
