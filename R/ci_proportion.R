#' Confidence Interval for a Population Proportion
#'
#' This function calculates confidence intervals for a population proportion. By default, "Clopper-Pearson" confidence intervals are calculated (via \code{stats::binom.test}). Further possibilities are "Wilson" and "Agresti-Coull" intervals. Bootstrap confidence intervals (not recommended but added for consistency) are calculated by the package "resample". The default bootstrap type is "BootstrapT".
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom stats qnorm binom.test
#' @importFrom resample bootstrap
#' @param x A numeric vector of 0 and 1 or the number of successes.
#' @param n The sample size. Only needed if \code{x} is a vector of length 1.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "Clopper-Pearson" (the default), "Agresti–Coull", "Wilson", "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{resample::CI.boot_type}.
#' @return A list with class \code{cint} containing these components:
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
#' x <- rep(0:1, times = c(50, 100))
#' ci_proportion(x)
#' ci_proportion(x, type = "Wilson")
#' ci_proportion(x, type = "Agresti")
#' ci_proportion(x, type = "boot", R = 1000)
#' @references
#' \enumerate{
#'   \item Clopper, C. and Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika. 26 (4).
#'   \item Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association, 22 (158).
#'   \item Agresti, A. and Coull, B. A. (1998). Approximate is better than 'exact' for interval estimation of binomial proportions. The American Statistician, 52 (2).
#' }
#'
ci_proportion <- function(x, n = NULL, probs = c(0.025, 0.975),
                          type = c("Clopper-Pearson", "Agresti-Coull", "Wilson", "bootstrap"),
                          boot_type = c("bootstrapT", "percentile", "t", "bca"),
                          R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)

  if (is.numeric(x) && length(x) == 1L) {
    stopifnot(!is.null(n), n >= x)
  } else if (is.numeric(x) && length(x) >= 1L) {
    x <- x[!is.na(x)]
    stopifnot(all(x == 0 || x == 1))
    n <- length(x)
    x <- sum(x)
  } else {
    stop("x must be either a binary vector or a single integer.")
  }

  # Calculate CI
  if (type != "bootstrap") {
    alpha <- 1 - diff(probs)
    if (type == "Clopper-Pearson") {
      cint <- binom.test(x, n = n, alternative = probs2alternative(probs),
                        conf.level = 1 - alpha)$conf.int
    } else if (type %in% c("Wilson", "Agresti-Coull")) {
      if (is_onesided(probs)) {
        alpha <- 2 * alpha
      } else if (!is_symmetric(probs)) {
        asymmetric_stop()
      }
      z <- qnorm(1 - alpha / 2)
      nt <- n + z^2
      pt <- (x + z^2 / 2) / nt
      if (type == "Wilson") {
        cint <- pt + c(-1, 1) * z / nt * sqrt(x * (n - x) / n + z^2 / 4)
      } else {
        cint <- pt + c(-1, 1) * z * sqrt(pt / nt * (1 - pt))
      }
    }
  } else { # bootstrap
    X <- rep(0:1, times = c(n - x, x))
    if (boot_type == "bootstrapT") {
      S <- bootstrap(X, statistic = c(mean = mean(X), sd = se_proportion(X)),
                     R = R, seed = seed)
    } else {
      S <- bootstrap(X, statistic = mean, R = R, seed = seed)
    }
    cint <- ci_boot(S, boot_type, probs, ...)
  }

  # Organize output
  cint <- check_output(cint, probs, c(0, 1))
  out <- list(parameter = "true proportion",
              interval = cint, estimate = x / n,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
