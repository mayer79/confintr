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
#' Calculates sample skewness, i.e., the third central moment. A value of 0 refers to
#' a perfectly symmetric empirical distribution.
#' @param x A numeric vector.
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
#' @param x A numeric vector.
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
#' This function calculates bootstrap confidence intervals for the population skewness,
#' see Details.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references.
#' The default bootstrap type is \code{"bca"} (bias-corrected accelerated) as it enjoys
#' the property of being second order accurate as well as transformation respecting
#' (see Efron, p. 188).
#' @param x A numeric vector.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. Currently not used as the only type is \code{"bootstrap"}.
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: The parameter in question.
#'   \item \code{interval}: The confidence interval for the parameter.
#'   \item \code{estimate}: The estimate for the parameter.
#'   \item \code{probs}: A vector of error probabilities.
#'   \item \code{type}: The type of the interval.
#'   \item \code{info}: An additional description text for the interval.
#' }
#' @export
#' @examples
#' x <- 1:20
#' ci_skewness(x, R = 999)  # In practice, use larger R
#' @seealso \code{\link{moments}}, \code{\link{kurtosis}}.
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_skewness <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Calculate CI
  x <- x[!is.na(x)]
  check_bca(boot_type, length(x), R)
  set_seed(seed)
  S <- boot::boot(x, statistic = function(x, id) skewness(x[id]), R = R, ...)
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  out <- list(
    parameter = "population skewness",
    interval = cint,
    estimate = skewness(x),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type, R)
  )
  class(out) <- "cint"
  out
}

#' CI for the Kurtosis
#'
#' This function calculates bootstrap confidence intervals for the population kurtosis,
#' see Details. Note that we use the version of the kurtosis that equals 3 under a
#' normal distribution.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references.
#' The default bootstrap type is \code{"bca"} (bias-corrected accelerated) as it enjoys
#' the property of being second order accurate as well as transformation respecting
#' (see Efron, p. 188).
#' @param x A numeric vector.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. Currently not used as the only type
#' is \code{"bootstrap"}.
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: The parameter in question.
#'   \item \code{interval}: The confidence interval for the parameter.
#'   \item \code{estimate}: The estimate for the parameter.
#'   \item \code{probs}: A vector of error probabilities.
#'   \item \code{type}: The type of the interval.
#'   \item \code{info}: An additional description text for the interval.
#' }
#' @export
#' @examples
#' x <- 1:20
#' ci_kurtosis(x, R = 999)  # In practice, use larger R
#' @seealso \code{\link{moments}}, \code{\link{skewness}}.
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_kurtosis <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Calculate CI
  x <- x[!is.na(x)]
  check_bca(boot_type, length(x), R)
  set_seed(seed)
  S <- boot::boot(x, statistic = function(x, id) kurtosis(x[id]), R = R, ...)
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  out <- list(
    parameter = "population kurtosis",
    interval = cint,
    estimate = kurtosis(x),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type, R)
  )
  class(out) <- "cint"
  out
}
