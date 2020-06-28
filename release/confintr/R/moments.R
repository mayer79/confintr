#' Moments
#'
#' Functions to calculate moments, skewness, and Pearson's measure of kurtosis. The latter is defined as the ratio of the 4th central moment and the squared second central moment. For a theoretical normal distribution, the kurtosis equals 3.
#'
#' @name moments
#' @param z Numeric vector.
#' @param p Order of moment.
#' @param central Should central moment be calculated? Default is \code{TRUE}.
#' @param na.rm Should missing values be removed? Default is \code{TRUE}. Otherwise, the result will be \code{NA} if missing values are present.
#' @return A numeric vector of length one.
#' @examples
#' x <- 1:100
#' moment(x, 4)
#' skewness(x)
#' kurtosis(x)
NULL

#' @rdname moments
#' @export
moment <- function(z, p = 1, central = TRUE, na.rm = TRUE) {
  if (na.rm) {
    z <- z[!is.na(z)]
  }
  if (central) {
    z <- z - mean(z)
  }
  sum(z^p) / length(z)
}

#' @rdname moments
#' @export
skewness <- function(z, na.rm = TRUE) {
  moment(z, p = 3, na.rm = na.rm) / moment(z, p = 2, na.rm = na.rm)^(3 / 2)
}

#' @rdname moments
#' @export
kurtosis <- function(z, na.rm = TRUE) {
  moment(z, p = 4, na.rm = na.rm) / moment(z, p = 2, na.rm = na.rm)^2
}

