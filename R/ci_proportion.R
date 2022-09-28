#' Confidence Interval for a Population Proportion
#'
#' This function calculates confidence intervals for a population proportion. By default, "Clopper-Pearson" confidence intervals are calculated (via \code{stats::binom.test}). Further possibilities are "Wilson", "Agresti-Coull", and "bootstrap" (mainly added for consistency and didactic purposes).
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type for the proportion is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' Note that we use the formula in \url{https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval} which does not simplify the 0.975 quantile of the normal by 2 as sometimes in other references.
#' @importFrom stats qnorm binom.test
#' @importFrom boot boot
#' @param x A numeric vector of 0 and 1 or the number of successes.
#' @param n The sample size. Only needed if \code{x} is a vector of length 1.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "Clopper-Pearson" (the default), "Agresti–Coull", "Wilson", "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "stud", "norm", "basic"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{boot::boot}.
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
#' ci_proportion(x, type = "Agresti-Coull")
#' ci_proportion(x, type = "bootstrap", R = 999)
#' @references
#' \enumerate{
#'   \item Clopper, C. and Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika. 26 (4).
#'   \item Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association, 22 (158).
#'   \item Agresti, A. and Coull, B. A. (1998). Approximate is better than 'exact' for interval estimation of binomial proportions. The American Statistician, 52 (2).
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_proportion <- function(x, n = NULL, probs = c(0.025, 0.975),
                          type = c("Clopper-Pearson", "Agresti-Coull", "Wilson", "bootstrap"),
                          boot_type = c("bca", "perc", "stud", "norm", "basic"),
                          R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Distinguish input
  if (is.numeric(x) && length(x) == 1L) {
    stopifnot(!is.null(n), n >= x)
  } else if (is.numeric(x) && length(x) >= 1L) {
    x <- x[!is.na(x)]
    stopifnot(all(x %in% 0:1))
    n <- length(x)
    x <- sum(x)
  } else {
    stop("x must be either a binary vector or a single integer.")
  }

  # Estimate
  estimate <- x / n

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
    x <- rep(0:1, times = c(n - x, x))
    check_bca(boot_type, n, R)
    set_seed(seed)
    S <- boot(x, statistic = function(x, id) c(mean(x[id]), se_proportion(x[id])^2), R = R, ...)
    cint <- ci_boot(S, boot_type, probs)
  }

  # Organize output
  cint <- check_output(cint, probs, c(0, 1))
  out <- list(parameter = "true proportion",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
