#' CI for a Population Quantile
#'
#' This function calculates CIs for a population quantile. By default, distribution-free
#' CIs based on the binomial distribution are calculated, see Hahn and Meeker.
#' Alternatively, bootstrap CIs are available (by default "bca").
#'
#' @param x A numeric vector.
#' @param q A single probability value determining the quantile.
#' Set to 0.5 for the median (the default).
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric 95% CI.
#' @param type Type of CI. One of "binomial" (default), or "bootstrap".
#' @param boot_type Type of bootstrap CI ("bca", "perc", "norm", "basic").
#' Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: The parameter in question.
#'   \item \code{interval}: The CI for the parameter.
#'   \item \code{estimate}: The estimate for the parameter.
#'   \item \code{probs}: A vector of error probabilities.
#'   \item \code{type}: The type of the interval.
#'   \item \code{info}: An additional description text for the interval.
#' }
#' @export
#' @examples
#' x <- 1:100
#' ci_quantile(x, q = 0.25)
#' @references
#' Hahn, G. and Meeker, W. (1991). Statistical Intervals. Wiley 1991.
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
