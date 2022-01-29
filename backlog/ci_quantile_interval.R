#' Confidence Interval for a Population Quantile
#'
#' This function calculates confidence intervals for a population quantile. By default, distribution-free confidence intervals based on the binomial distribution are formed, see Hahn and Meeker. Alternatively, bootstrap confidence intervals are available.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' @importFrom stats pbeta
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
#' ci_quantile_region(x)
#' tolerance::nptol.int(x, P = 0.8, side = 2)
#' ci_quantile_region(x, probs = c(0, 0.95))
#' tolerance::nptol.int(x, P = 0.8, side = 1)
#' ci_quantile_region(x, probs = c(0.05, 1))
#' @references
#' \enumerate{
#'   \item Hahn, G. and Meeker, W. (1991). Statistical Intervals. Wiley 1991.
#'   \item Wilks, S. S. (1941). Determination of Sample Sizes for Setting Tolerance Limits. The Annals of Mathematical Statistics, 12.
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
#' @seealso \code{\link{ci_quantile}}.
ci_quantile_region <- function(x, q = 0.8, probs = c(0.025, 0.975),
                                 type = c("Wilks", "bootstrap"),
                                 boot_type = c("bca", "perc", "norm", "basic"),
                                 R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)
  stopifnot(length(q) == 1L, q > 0, q < 1)
  limits <- c(-Inf, Inf)

  # Remove NAs
  x <- x[!is.na(x)]
  n <- length(x)
  cint <- limits

  # Calculate CI
  if (type == "Wilks") {
    side <- probs2alternative(probs)
    if (side == "two.sided") {
      estimate <- quantile(x, probs = c((1 - q) / 2, 1 - (1 - q) / 2))
      r <- seq_len((n + 1) %/% 2)
      candidates <- pbeta(q, n - 2 * r + 1, 2 * r, lower.tail = FALSE)
      ok <- which(candidates > diff(probs))
      if (length(ok)) {
        r <- max(ok)
        s <- n - r + 1
        cint <- sort(x)[c(r, s)]
      }
    } else if (side == "greater") {
      estimate <- quantile(x, probs = c((1 - q) / 2, 1))
      cint[1] <- ci_quantile(x, q = 1 - q, probs = probs)$interval[1]
    } else {
      estimate <- quantile(x, probs = c(0, 1 - (1 - q) / 2))
      cint[2] <- ci_quantile(x, q = q, probs = probs)$interval[2]
    }
  }
  # } else { # Bootstrap not implemented yet
  #   check_bca(boot_type, n, R)
  #   set_seed(seed)
  #   S <- boot(x, statistic = function(x, id) quantile(x[id], probs = q, names = FALSE),
  #             R = R, ...)
  #   cint <- ci_boot(S, boot_type, probs)
  # }

  # Organize output
  cint <- check_output(cint, probs, limits)
  out <- list(parameter = sprintf("population %s quantile region", format_p(q)),
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
