#' Standard errors
#'
#' Functions to calculate standard errors of different statistics.
#' The availability of a standard error (or statistic proportional to it) allows to
#' apply "stud" (bootstrap t) bootstrap.
#'
#' @name se
#' @param z Numeric vector.
#' @param y Numeric vector.
#' @param na.rm Should missing values be removed before calculation? Default is `TRUE`.
#' @param var.equal Should the variances be treated as being equal? Default is `FALSE`.
#' @param ... Further arguments to be passed from other methods.
#' @returns A numeric vector of length one.
#' @examples
#' se_mean(1:100)
NULL

#' @rdname se
#' @export
se_mean <- function(z, na.rm = TRUE, ...) {
  if (na.rm) {
    z <- z[!is.na(z)]
  }
  n <- length(z)
  if (n < 1L) {
    stop("Not enough non-missing observations to calculate standard error.")
  }
  sqrt(stats::var(z) / n)
}

#' @rdname se
#' @export
se_mean_diff <- function(z, y, na.rm = TRUE, var.equal = FALSE, ...) {
  if (na.rm) {
    z <- z[!is.na(z)]
    y <- y[!is.na(y)]
  }
  stats::t.test(z, y, var.equal = var.equal)$stderr
}

#' @rdname se
#' @export
se_var <- function(z, na.rm = TRUE, ...) {
  # S.S. Wilks (1962), Mathematical Statistics, Wiley & Sons.
  if (na.rm) {
    z <- z[!is.na(z)]
  }
  n <- length(z)
  if (n <= 3L) {
    stop("Not enough non-missing observations to calculate standard error.
         Need at least four.")
  }
  sqrt((1 / n) * (moment(z, p = 4) - (n - 3) / (n - 1) * stats::var(z)^2))
}

#' @rdname se
#' @export
se_proportion <- function(z, na.rm = TRUE, ...) {
  if (na.rm) {
    z <- z[!is.na(z)]
  }
  p <- mean(z)
  n <- length(z)
  if (n < 1L) {
    stop("Not enough non-missing observations to calculate standard error.")
  }
  sqrt(p * (1 - p) / n)
}

# SE(sample standard dev)
# https://stats.stackexchange.com/questions/631/standard-deviation-of-standard-deviation
