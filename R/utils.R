#' @title two_column_check
#'
#' @description Help function to check arguments.
#'
#' @param x data.frame, xts or data.table object
#'
#' @return data.table with 2 columns: Datetime and Value
#'
#' @import data.table
#'
#' @examples
#'  data(spy)
#'  two_column_check(subset(spy, select = c("index", "close")))
#'  \dontrun{two_column_check(subset(spy, select = c("index")))}
#'  \dontrun{two_column_check(subset(spy, select = c("close")))}
#' @export
two_column_check <- function(x) {

  # convert to data.table
  if (is.data.frame(x)) {
    x <- as.data.table(x)
  } else {
    stop("Argument x must be of class data.frame")
  }

  # check if x contains POSIXct and numeric columns
  all_cols <- colnames(x)
  datetime_column <- x[ , lapply(.SD, inherits, "POSIXct"), .SDcols = all_cols]
  datetime_column <- unlist(datetime_column, use.names = FALSE)
  if (length(datetime_column) == 0) {
    stop('Argument price must contain POSIXct column.')
  }
  numeric_column <- which(sapply(x, inherits, "numeric"))
  if (length(numeric_column) != 1) {
    stop('Argument price must contain 1 numeric column.')
  }

  # rename columns if exists
  colnames(x)[datetime_column] <- 'Datetime'
  colnames(x)[numeric_column] <- 'Value'

  # set key
  setkey(x, 'Datetime')

  return(x)
}


#' @title one_column_check
#'
#' @description Help function to check arguments of x with one column.
#'
#' @param x data.frame, xts or data.table object
#'
#' @return data.table with 1 column: Datetime
#'
#' @import data.table
#'
#' @examples
#'  data(spy)
#'  one_column_check(subset(spy, select = c("index")))
#'  \dontrun{one_column_check(subset(spy, select = c("close")))}
#'
#' @export
one_column_check <- function(x) {
  # convert to data.table
  if (is.data.frame(x)) {
    x <- data.table::as.data.table(x)
  } else if (inherits(x, 'POSIXct')) {
    x <- data.table::as.data.table(data.frame(Datetime = x))
  }

  # check if x contains POSIXct
  if (!inherits(x[[1]], 'POSIXct')) {
    stop('Argument must be POSIXct column/vector.')
  }

  # rename columns if exists
  colnames(x) <- 'Datetime'

  # set key
  data.table::setkey(x, 'Datetime')

  return(x)
}
