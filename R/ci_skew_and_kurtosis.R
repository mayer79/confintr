#' Sample Moments
#'
#' Calculates central or non-central sample moments.
#'
#' @param z A numeric vector.
#' @param p Order of moment.
#' @param central Should central moment be calculated? Default is \code{TRUE}.
#' @param na.rm Logical flag indicating whether to remove missing values or not.
#' Default is \code{TRUE}.
#' @return Numeric vector of length 1.
#' @export
#' @examples
#' moment(1:10, p = 1)
#' moment(1:10, p = 1, central = FALSE)
#' moment(1:10, p = 2) / stats::var(1:10)
#' @seealso \code{\link{skewness}}, \code{\link{kurtosis}}.
moment <- function(z, p = 1, central = TRUE, na.rm = TRUE) {
  if (na.rm) {
    z <- z[!is.na(z)]
  }
  if (central) {
    z <- z - mean(z)
  }
  sum(z^p) / length(z)
}

#' Sample Skewness
#'
#' Calculates sample skewness. A value of 0 refers to a perfectly symmetric distribution.
#'
#' @param z A numeric vector.
#' @param na.rm Logical flag indicating whether to remove missing values or not.
#' Default is \code{TRUE}.
#' @return Numeric vector of length 1.
#' @export
#' @examples
#' skewness(1:10)
#' skewness(rexp(100))
#' @seealso \code{\link{moment}}
skewness <- function(z, na.rm = TRUE) {
  moment(z, p = 3, na.rm = na.rm) / moment(z, p = 2, na.rm = na.rm)^(3 / 2)
}

#' Pearson's Measure of Kurtosis
#'
#' Defined as the ratio of the 4th central moment and the squared
#' second central moment. Under perfect normality, the kurtosis equals 3.
#' Put differently, we do not show "excess kurtosis" but rather kurtosis.
#'
#' @param z A numeric vector.
#' @param na.rm Logical flag indicating whether to remove missing values or not.
#' Default is \code{TRUE}.
#' @return Numeric vector of length 1.
#' @export
#' @examples
#' kurtosis(1:10)
#' kurtosis(rnorm(1000))
#' @seealso \code{\link{moment}}
kurtosis <- function(z, na.rm = TRUE) {
  moment(z, p = 4, na.rm = na.rm) / moment(z, p = 2, na.rm = na.rm)^2
}

#' CI for the Skewness
#'
#' This function calculates bootstrap CIs for the population skewness.
#' By default, bootstrap type "bca" is used.
#'
#' @param x A numeric vector.
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @param type Type of CI. Currently not used as the only type is \code{"bootstrap"}.
#' @param boot_type Type of bootstrap CI ("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: Parameter specification.
#'   \item \code{interval}: CI for the parameter.
#'   \item \code{estimate}: Parameter estimate.
#'   \item \code{probs}: Lower and upper probabilities.
#'   \item \code{type}: Type of interval.
#'   \item \code{info}: Additional description.
#' }
#' @export
#' @examples
#' x <- 1:20
#' ci_skewness(x, R = 999)  # Use larger R
#' @seealso \code{\link{kurtosis}}
ci_skewness <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Calculate CI
  x <- x[!is.na(x)]
  check_bca(boot_type, n = length(x), R = R)
  set_seed(seed)
  S <- boot::boot(x, statistic = function(x, id) skewness(x[id]), R = R, ...)
  cint <- ci_boot(S, boot_type = boot_type, probs = probs)

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(-Inf, Inf))
  out <- list(
    parameter = "population skewness",
    interval = cint,
    estimate = skewness(x),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}

#' CI for the Kurtosis
#'
#' This function calculates bootstrap CIs for the population kurtosis.
#' Note that we use the version of the kurtosis that equals 3 under a
#' normal distribution, i.e., we are not calculating the excess kurtosis.
#' By default, bootstrap type "bca" is used.
#'
#' @param x A numeric vector.
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @param type Type of CI. Currently not used as the only type is \code{"bootstrap"}.
#' @param boot_type Type of bootstrap CI ("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: Parameter specification.
#'   \item \code{interval}: CI for the parameter.
#'   \item \code{estimate}: Parameter estimate.
#'   \item \code{probs}: Lower and upper probabilities.
#'   \item \code{type}: Type of interval.
#'   \item \code{info}: Additional description.
#' }
#' @export
#' @examples
#' x <- 1:20
#' ci_kurtosis(x, R = 999)  # Use larger R
#' @seealso \code{\link{skewness}}
ci_kurtosis <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Calculate CI
  x <- x[!is.na(x)]
  check_bca(boot_type, n = length(x), R = R)
  set_seed(seed)
  S <- boot::boot(x, statistic = function(x, id) kurtosis(x[id]), R = R, ...)
  cint <- ci_boot(S, boot_type = boot_type, probs = probs)

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(-Inf, Inf))
  out <- list(
    parameter = "population kurtosis",
    interval = cint,
    estimate = kurtosis(x),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}
