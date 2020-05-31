#' Confidence Interval for the Median Absolute Deviation
#'
#' This function calculates bootstrap confidence intervals for the population median absolute deviation, see \code{stats::mad} for more information on this measure of scale.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' @importFrom boot boot
#' @importFrom stats mad
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param constant Scaling factor applied. The default (1.4826) ensures that the MAD equals the standard deviation for a theoretical normal distribution.
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
#' ci_mad(x, R = 999)
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_mad <- function(x, probs = c(0.025, 0.975), constant = 1.4826,
                   type = "bootstrap",
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
  S <- boot(x, statistic = function(x, id) mad(x[id], constant = constant), R = R, ...)
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs, c(0, Inf))
  out <- list(parameter = "population MAD",
              interval = cint, estimate = mad(x, constant = constant),
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
