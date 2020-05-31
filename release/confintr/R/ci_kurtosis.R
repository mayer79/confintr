#' Confidence Interval for the Kurtosis
#'
#' This function calculates bootstrap confidence intervals for the population kurtosis, see Details. Note that we use the version of the kurtosis that equals 3 for a theoretical normal distribution.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' @importFrom boot boot
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. Currently not used as the only type is "bootstrap".
#' @param boot_type Type of bootstrap confidence interval c("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
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
#' set.seed(1)
#' x <- rnorm(100)
#' ci_kurtosis(x, R = 999)
#' @seealso \code{\link{moments}}, \code{\link{ci_skewness}}.
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
  S <- boot(x, statistic = function(x, id) kurtosis(x[id]), R = R, ...)
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  out <- list(parameter = "population kurtosis",
              interval = cint, estimate = kurtosis(x),
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
