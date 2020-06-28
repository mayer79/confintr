#' Confidence Interval for a Population Quantile
#'
#' This function calculates confidence intervals for a population quantile. By default, distribution-free confidence intervals based on the binomial distribution are formed, see Hahn and Meeker. Alternatively, bootstrap confidence intervals are available.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' @importFrom stats quantile qbinom
#' @importFrom boot boot
#' @param x A numeric vector.
#' @param q A single probability value determining the quantile. Set to 0.5 for the median (the default).
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "binomial" (default), or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "norm", "basic"). Only used for \code{type = "bootstrap"}.
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
#' x <- 1:100
#' ci_quantile(x)
#' ci_quantile(x, q = 0.25)
#' ci_quantile(x, q = 0.25, type = "bootstrap", R = 999)
#' @references
#' \enumerate{
#'   \item Hahn, G. and Meeker, W. (1991). Statistical Intervals. Wiley 1991.
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
#' @seealso \code{\link{ci_quantile}}.
ci_quantile <- function(x, q = 0.5, probs = c(0.025, 0.975),
                        type = c("binomial", "bootstrap"),
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)
  stopifnot(length(q) == 1L, q > 0, q < 1)
  limits <- c(-Inf, Inf)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- quantile(x, probs = q, names = FALSE)
  n <- length(x)

  # Calculate CI
  if (type == "binomial") {
    k <- qbinom(probs, n, q) + 0:1
    x <- sort(x)
    cint <- limits
    if (k[1] >= 1) {
      cint[1] <- x[k[1]]
    }
    if (k[2] <= n) {
      cint[2] <- x[k[2]]
    }
  } else { # Bootstrap
    check_bca(boot_type, n, R)
    set_seed(seed)
    S <- boot(x, statistic = function(x, id) quantile(x[id], probs = q, names = FALSE),
              R = R, ...)
    cint <- ci_boot(S, boot_type, probs)
  }

  # Organize output
  cint <- check_output(cint, probs, limits)
  out <- list(parameter = sprintf("population %s quantile", format_p(q)),
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
