% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labeling_fixed_time.R
\name{labeling_fixed_time}
\alias{labeling_fixed_time}
\title{labeling_fixed_time}
\usage{
labeling_fixed_time(prices, threshold = 0.01, horizont = 1L)
}
\arguments{
\item{prices}{vector of prices}

\item{threshold}{double, if return greater (lower )than threshold than  1 (-1), other 0}

\item{horizont}{integer, number of steps to the future}
}
\value{
vector of labels
}
\description{
Labeling time series data on fixed horizont
  Originally described in the book Advances in Financial Machine Learning, Chapter 3.2, p.43-44.
}
\examples{
data("spy")
head(labeling_fixed_time(spy$close, 0.05, 10000))

}
