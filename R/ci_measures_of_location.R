#' CI for the Population Mean
#'
#' This function calculates CIs for the population mean. By default, Student's t method
#' is used. Alternatively, Wald and bootstrap CIs are available.
#'
#' The default bootstrap type for the mean is "stud" (bootstrap t) as it enjoys the
#' property of being second order accurate and has a stable variance estimator
#' (see Efron, p. 188).
#'
#' @param x A numeric vector.
#' @param probs Lower and upper probabilities, by default `c(0.025, 0.975)`.
#' @param type Type of CI. One of "t" (default), "Wald", or "bootstrap".
#' @param boot_type Type of bootstrap CI. Only used for `type = "bootstrap"`.
#' @param R The number of bootstrap resamples. Only used for `type = "bootstrap"`.
#' @param seed An integer random seed. Only used for `type = "bootstrap"`.
#' @param ... Further arguments passed to [boot::boot()].
#' @returns
#'   An object of class "cint" containing these components:
#'   - `parameter`: Parameter specification.
#'   - `interval`: CI for the parameter.
#'   - `estimate`: Parameter estimate.
#'   - `probs`: Lower and upper probabilities.
#'   - `type`: Type of interval.
#'   - `info`: Additional description.
#' @export
#' @examples
#' x <- 1:100
#' ci_mean(x)
#' ci_mean(x, type = "bootstrap", R = 999, seed = 1)  # Use larger R
#' @references
#'   1. Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in
#'     the Social Sciences. New York, NY: Sage Publications.
#'   2. Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
ci_mean <- function(x, probs = c(0.025, 0.975), type = c("t", "Wald", "bootstrap"),
                    boot_type = c("stud", "bca", "perc", "norm", "basic"),
                    R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- mean(x)

  # Calculate CI
  if (type %in% c("t", "Wald")) {
    q <- if (type == "t") stats::qt(probs, df = length(x) - 1L) else stats::qnorm(probs)
    se <- se_mean(x)
    if (se < 10 * .Machine$double.eps * abs(estimate)) {
      stop("Data essentially constant")
    }
    cint <- estimate + se * q
  } else if (type == "bootstrap") {
    check_bca(boot_type, length(x), R)
    set_seed(seed)
    S <- boot::boot(
      x, statistic = function(x, id) c(mean(x[id]), se_mean(x[id])^2), R = R, ...
    )
    cint <- ci_boot(S, boot_type, probs)
  }

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(-Inf, Inf))
  out <- list(
    parameter = "population mean",
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}

#' CI for a Population Quantile
#'
#' This function calculates CIs for a population quantile. By default, distribution-free
#' CIs based on the binomial distribution are calculated, see Hahn and Meeker.
#' Alternatively, bootstrap CIs are available (default "bca").
#'
#' @inheritParams ci_mean
#' @param q A single probability value determining the quantile (0.5 for median).
#' @param type Type of CI. One of "binomial" (default), or "bootstrap".
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' x <- 1:100
#' ci_quantile(x, q = 0.25)
#' @references
#'   Hahn, G. and Meeker, W. (1991). Statistical Intervals. Wiley 1991.
#' @seealso [ci_median()]
ci_quantile <- function(x, q = 0.5, probs = c(0.025, 0.975),
                        type = c("binomial", "bootstrap"),
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)
  stopifnot(length(q) == 1L, q > 0, q < 1)
  limits <- c(-Inf, Inf)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- stats::quantile(x, probs = q, names = FALSE)
  n <- length(x)

  # Calculate CI
  if (type == "binomial") {
    k <- stats::qbinom(probs, n, q) + 0:1
    x <- sort(x)
    cint <- limits
    if (k[1L] >= 1) {
      cint[1L] <- x[k[1L]]
    }
    if (k[2L] <= n) {
      cint[2L] <- x[k[2L]]
    }
  } else { # Bootstrap
    check_bca(boot_type, n = n, R = R)
    set_seed(seed)
    S <- boot::boot(
      x,
      statistic = function(x, id) stats::quantile(x[id], probs = q, names = FALSE),
      R = R,
      ...
    )
    cint <- ci_boot(S, boot_type = boot_type, probs = probs)
  }

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = limits)
  out <- list(
    parameter = sprintf("population %s quantile", format_p(q)),
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}

#' CI for the Population Median
#'
#' This function calculates CIs for the population median by calling [ci_quantile()].
#'
#' @inheritParams ci_quantile
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' ci_median(1:100)
#' @seealso [ci_quantile()]
ci_median <- function(x, probs = c(0.025, 0.975),
                      type = c("binomial", "bootstrap"),
                      boot_type = c("bca", "perc", "norm", "basic"),
                      R = 9999L, seed = NULL, ...) {
  out <- ci_quantile(
    x,
    q = 0.5,
    probs = probs,
    type = type,
    boot_type = boot_type,
    R = R,
    seed = seed,
    ...
  )
  out$parameter <- "population median"
  out
}
