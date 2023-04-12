#' CI for the Population Variance
#'
#' This function calculates confidence intervals for the population variance.
#' By default, classic confidence intervals are calculated based on the chi-squared
#' distribution, assuming normal distribution (see Smithson).
#' Bootstrap confidence intervals are also available. We recommend them for the non-normal case.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references.
#' The default bootstrap type is \code{bca} (bias-corrected accelerated) as it enjoys
#' the property of being second order accurate as well as transformation respecting
#' (see Efron, p. 188).
#' The \code{stud} (bootstrap t) bootstrap uses a general formula for the standard error
#' of the sample variance given in Wilks.
#' @param x A numeric vector.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric
#' 95% confidence interval.
#' @param type Type of confidence interval. One of \code{"chi-squared"} (default) or
#' \code{"bootstrap"}.
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "stud", "norm", "basic").
#' Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{boot::boot}.
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
#' x <- 1:100
#' ci_var(x)
#' ci_var(x, type = "bootstrap", R = 999)
#' @references
#' \enumerate{
#'   \item Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#'   \item S.S. Wilks (1962), Mathematical Statistics, Wiley & Sons.
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
#' @seealso \code{\link{ci_sd}}.
ci_var <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                   boot_type = c("bca", "perc", "stud", "norm", "basic"),
                   R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- var(x)
  n <- length(x)

  # Calculate CI
  if (type == "chi-squared") {
    cint <- estimate * (n - 1L) / stats::qchisq(1 - probs, df = n - 1L)
  } else if (type == "bootstrap") {
    check_bca(boot_type, n, R)
    set_seed(seed)
    S <- boot::boot(
      x, statistic = function(x, id) c(stats::var(x[id]), se_var(x[id])^2), R = R, ...
    )
    cint <- ci_boot(S, boot_type, probs)
  }
  cint <- check_output(cint, probs, c(0, Inf))

  # Organize output
  out <- list(
    parameter = "population variance",
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = type,
    info = boot_info(type, boot_type, R)
  )
  class(out) <- "cint"
  out
}

#' CI for the Population Std
#'
#' This function calculates confidence intervals for the population standard deviation.
#' They are derived from confidence intervals for the variance by taking the square-root.
#' For details, see \code{\link{ci_var}}.
#' @param x A numeric vector.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of \code{"chi-squared"} (default)
#' or \code{"bootstrap"}.
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "stud", "norm", "basic").
#' Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{boot::boot}.
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
#' x <- 1:100
#' sd(x)
#' ci_sd(x)
#' sqrt(ci_var(x)$interval)
#' ci_sd(x, type = "bootstrap", R = 999)
#' @seealso \code{\link{ci_var}}.
ci_sd <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                  boot_type = c("bca", "perc", "stud", "norm", "basic"),
                  R = 9999, seed = NULL, ...) {
  out <- ci_var(x = x, probs = probs, type = type, boot_type = boot_type,
                R = R, seed = seed, ...)
  out$estimate <- sqrt(out$estimate)
  out$interval <- sqrt(out$interval)
  out$parameter <- "population standard deviation"
  out
}

#' CI for the IQR
#'
#' This function calculates bootstrap confidence intervals for the
#' population interquartile range (IQR), i.e., the difference between first and third quartile.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references.
#' The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the
#' property of being second order accurate as well as transformation respecting
#' (see Efron, p. 188).
#' @param x A numeric vector.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric
#' 95% confidence interval.
#' @param type Type of confidence interval. Currently not used as the only type is \code{"bootstrap"}.
#' @param boot_type Type of bootstrap confidence interval c("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{boot::boot}.
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
#' set.seed(1)
#' x <- rnorm(100)
#' ci_IQR(x, R = 999)
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_IQR <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
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
  S <- boot::boot(x, statistic = function(x, id) stats::IQR(x[id]), R = R, ...)
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs, c(0, Inf))
  out <- list(
    parameter = "population IQR",
    interval = cint,
    estimate = stats::IQR(x),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type, R)
  )
  class(out) <- "cint"
  out
}

#' CI for the MAD
#'
#' This function calculates bootstrap confidence intervals for the population median
#' absolute deviation (MAD), see \code{stats::mad} for more information on this measure of scale.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references.
#' The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the
#' property of being second order accurate as well as transformation respecting
#' (see Efron, p. 188).
#' @param x A numeric vector.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric
#' 95% confidence interval.
#' @param constant Scaling factor applied. The default (1.4826) ensures that the MAD
#' equals the standard deviation for a theoretical normal distribution.
#' @param type Type of confidence interval. Currently not used as the only type is "bootstrap".
#' @param boot_type Type of bootstrap confidence interval c("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{boot::boot}.
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
#' set.seed(1)
#' x <- rnorm(100)
#' ci_mad(x, R = 999)
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_mad <- function(x, probs = c(0.025, 0.975), constant = 1.4826,
                   type = "bootstrap", boot_type = c("bca", "perc", "norm", "basic"),
                   R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Calculate CI
  x <- x[!is.na(x)]
  check_bca(boot_type, length(x), R)
  set_seed(seed)
  S <- boot::boot(
    x, statistic = function(x, id) stats::mad(x[id], constant = constant), R = R, ...
  )
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs, c(0, Inf))
  out <- list(
    parameter = "population MAD",
    interval = cint,
    estimate = stats::mad(x, constant = constant),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type, R)
  )
  class(out) <- "cint"
  out
}
