% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moments.R
\name{moments}
\alias{moments}
\alias{moment}
\alias{skewness}
\alias{kurtosis}
\title{Moments}
\usage{
moment(z, p = 1, central = TRUE, na.rm = TRUE)

skewness(z, na.rm = TRUE)

kurtosis(z, na.rm = TRUE)
}
\arguments{
\item{z}{Numeric vector.}

\item{p}{Order of moment.}

\item{central}{Should central moment be calculated? Default is \code{TRUE}.}

\item{na.rm}{Should missing values be removed? Default is \code{TRUE}. Otherwise, the result will be \code{NA} if missing values are present.}
}
\value{
A numeric vector of length one.
}
\description{
Functions to calculate moments, skewness, and Pearson's measure of kurtosis. The latter is defined as the ratio of the 4th central moment and the squared second central moment. For a theoretical normal distribution, the kurtosis equals 3.
}
\examples{
x <- 1:100
moment(x, 4)
skewness(x)
kurtosis(x)
}
