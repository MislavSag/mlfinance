#' @title two_column_check
#'
#' @description Help function to check arguments.
#'
#' @param df data.frame, xts or data.table object
#'
#' @return data.table with 2 columns: Datetime and Value
#'
#' @examples
#'  data(spy)
#'  two_column_check(subset(spy, select = c("index", "close")))
#'  \dontrun{two_column_check(subset(spy, select = c("index")))}
#'  \dontrun{two_column_check(subset(spy, select = c("close")))}
#' @export
two_column_check <- function(df) {

  # convert to data.table
  if (xts::is.xts(df) | is.data.frame(df)) {
    df <- data.table::as.data.table(df)
  }

  # check if df contains POSIXct and numeric columns
  datetime_column <- which(sapply(df, inherits, "POSIXct"))
  if (length(datetime_column) == 0) {
    stop('Argument price must contain POSIXct column.')
  }
  numeric_column <- which(sapply(df, inherits, "numeric"))
  if (length(numeric_column) != 1) {
    stop('Argument price must contain 1 numeric column.')
  }

  # rename columns if exists
  colnames(df)[datetime_column] <- 'Datetime'
  colnames(df)[numeric_column] <- 'Value'

  # set key
  data.table::setkey(df, 'Datetime')

  return(df)
}


#' @title one_column_check
#'
#' @description Help function to check arguments of df with one column.
#'
#' @param df data.frame, xts or data.table object
#'
#' @return data.table with 1 column: Datetime
#'
#' @examples
#'  data(spy)
#'  one_column_check(subset(spy, select = c("index")))
#'  \dontrun{one_column_check(subset(spy, select = c("close")))}
#'
#' @export
one_column_check <- function(df) {
  # convert to data.table
  if (xts::is.xts(df) | is.data.frame(df)) {
    df <- data.table::as.data.table(df)
  } else if (inherits(df, 'POSIXct')) {
    df <- data.table::as.data.table(data.frame(Datetime = df))
  }

  # check if df contains POSIXct
  if (!inherits(df[[1]], 'POSIXct')) {
    stop('Argument must be POSIXct column/vector.')
  }

  # rename columns if exists
  colnames(df) <- 'Datetime'

  # set key
  data.table::setkey(df, 'Datetime')

  return(df)
}
