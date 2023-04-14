#' CI for a Population Proportion
#'
#' This function calculates CIs for a population proportion. By default,
#' "Clopper-Pearson" CIs are calculated (via \code{stats::binom.test()}).
#' Further possibilities are "Wilson" (without continuity correction),
#' "Agresti-Coull" (using normal quantile instead of +2 correction),
#' and "bootstrap" (by default "bca"). Note that the Agresti-Coull
#'
#' Note that we use the formulas for the Wilson and Agresti-Coull intervals in
#' \url{https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval}.
#' They agree with binom::binom.confint(x, n, method = "ac"/"wilson").
#'
#' @param x A numeric vector with one value (0/1) per observation,
#' or the number of successes.
#' @param n The sample size. Only needed if \code{x} is a vector of length 1.
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @param type Type of CI. One of "Clopper-Pearson" (the default), "Agresti–Coull",
#' "Wilson", "bootstrap".
#' @param boot_type Type of bootstrap CI ("bca", "perc", "stud", "norm", "basic").
#' Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
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
#' x <- rep(0:1, times = c(50, 100))
#' ci_proportion(x)
#' ci_proportion(x, type = "Wilson")
#' ci_proportion(x, type = "Agresti-Coull")
#' @references
#' \enumerate{
#'   \item Clopper, C. and Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika. 26 (4).
#'   \item Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association, 22 (158).
#'   \item Agresti, A. and Coull, B. A. (1998). Approximate is better than 'exact' for interval estimation of binomial proportions. The American Statistician, 52 (2).
#' }
ci_proportion <- function(x, n = NULL, probs = c(0.025, 0.975),
                          type = c("Clopper-Pearson", "Agresti-Coull", "Wilson", "bootstrap"),
                          boot_type = c("bca", "perc", "stud", "norm", "basic"),
                          R = 9999L, seed = NULL, ...) {
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
      cint <- stats::binom.test(
        x, n = n, alternative = probs2alternative(probs), conf.level = 1 - alpha
      )$conf.int
    } else if (type %in% c("Wilson", "Agresti-Coull")) {
      if (is_onesided(probs)) {
        alpha <- 2 * alpha
      } else if (!is_equal_tailed(probs)) {
        unequal_stop()
      }
      z <- stats::qnorm(1 - alpha / 2)
      nt <- n + z^2
      pt <- (x + z^2 / 2) / nt
      if (type == "Wilson") {
        cint <- pt + c(-1, 1) * z / nt * sqrt(x * (n - x) / n + z^2 / 4)
      } else {  # Agresti-Coull
        cint <- pt + c(-1, 1) * z * sqrt(pt / nt * (1 - pt))
      }
    }
  } else {  # bootstrap
    x <- rep(0:1, times = c(n - x, x))
    check_bca(boot_type, n = n, R = R)
    set_seed(seed)
    S <- boot::boot(
      x, statistic = function(x, id) c(mean(x[id]), se_proportion(x[id])^2), R = R, ...
    )
    cint <- ci_boot(S, boot_type = boot_type, probs = probs)
  }

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(0, 1))
  out <- list(
    parameter = "true proportion",
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}
