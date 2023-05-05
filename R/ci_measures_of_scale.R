#' CI for the Population Variance
#'
#' This function calculates CIs for the population variance.
#'
#' By default, classic CIs are calculated based on the chi-squared distribution,
#' assuming normal distribution (see Smithson). Bootstrap CIs are also available
#' (default: "bca"). We recommend them for the non-normal case.
#'
#' The `stud` (bootstrap t) bootstrap uses the standard error of the sample variance
#' given in Wilks.
#'
#' @inheritParams ci_mean
#' @param type Type of CI. One of `"chi-squared"` (default) or `"bootstrap"`.
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' x <- 1:100
#' ci_var(x)
#' ci_var(x, type = "bootstrap", R = 999)  # Use larger R
#' @references
#'   1. Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in
#'     the Social Sciences. New York, NY: Sage Publications.
#'   2. S.S. Wilks (1962), Mathematical Statistics, Wiley & Sons.
#' @seealso [ci_sd()]
ci_var <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                   boot_type = c("bca", "perc", "stud", "norm", "basic"),
                   R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- stats::var(x)
  n <- length(x)

  # Calculate CI
  if (type == "chi-squared") {
    cint <- estimate * (n - 1L) / stats::qchisq(1 - probs, df = n - 1L)
  } else if (type == "bootstrap") {
    check_bca(boot_type, n = n, R = R)
    set_seed(seed)
    S <- boot::boot(
      x, statistic = function(x, id) c(stats::var(x[id]), se_var(x[id])^2), R = R, ...
    )
    cint <- ci_boot(S, boot_type = boot_type, probs = probs)
  }
  cint <- check_output(cint, probs = probs, parameter_range = c(0, Inf))

  # Organize output
  out <- list(
    parameter = "population variance",
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}

#' CI for the Population Std
#'
#' This function calculates CIs for the population standard deviation.
#' They are derived from CIs for the variance by taking the square-root, see[ci_var()].
#'
#' @inheritParams ci_var
#' @inheritParams ci_mean
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' x <- 1:100
#' ci_sd(x)
#' ci_sd(x, type = "bootstrap", R = 999)  # Use larger R
#' @seealso [ci_var()]
ci_sd <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                  boot_type = c("bca", "perc", "stud", "norm", "basic"),
                  R = 9999L, seed = NULL, ...) {
  out <- ci_var(
    x = x, probs = probs, type = type, boot_type = boot_type, R = R, seed = seed, ...
  )
  out$estimate <- sqrt(out$estimate)
  out$interval <- sqrt(out$interval)
  out$parameter <- "population standard deviation"
  out
}

#' CI for the IQR
#'
#' This function calculates bootstrap CIs (by default "bca") for the population
#' interquartile range (IQR), i.e., the difference between first and third quartile.
#'
#' @inheritParams ci_mean
#' @param type Type of CI. Currently not used as the only type is `"bootstrap"`.
#' @param boot_type Type of bootstrap CI c("bca", "perc", "norm", "basic").
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' x <- rnorm(100)
#' ci_IQR(x, R = 999)  # Use larger R
ci_IQR <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
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
  S <- boot::boot(x, statistic = function(x, id) stats::IQR(x[id]), R = R, ...)
  cint <- ci_boot(S, boot_type = boot_type, probs = probs)

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(0, Inf))
  out <- list(
    parameter = "population IQR",
    interval = cint,
    estimate = stats::IQR(x),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}

#' CI for the MAD
#'
#' This function calculates bootstrap CIs (default: "bca") for the population median
#' absolute deviation (MAD), see [stats::mad()] for more information.
#'
#' @inheritParams ci_IQR
#' @inheritParams ci_mean
#' @param constant Scaling factor applied. The default (1.4826) ensures that the MAD
#'   equals the standard deviation for a theoretical normal distribution.
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' x <- rnorm(100)
#' ci_mad(x, R = 999)  # Use larger R
ci_mad <- function(x, probs = c(0.025, 0.975), constant = 1.4826,
                   type = "bootstrap", boot_type = c("bca", "perc", "norm", "basic"),
                   R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Calculate CI
  x <- x[!is.na(x)]
  check_bca(boot_type, n = length(x), R = R)
  set_seed(seed)
  S <- boot::boot(
    x, statistic = function(x, id) stats::mad(x[id], constant = constant), R = R, ...
  )
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(0, Inf))
  out <- list(
    parameter = "population MAD",
    interval = cint,
    estimate = stats::mad(x, constant = constant),
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}
