#' Confidence Interval for the Population Median Difference
#'
#' This function calculates bootstrap confidence intervals for the population value of median(x) - median(y).
#'
#' Bootstrap confidence intervals are calculated by the package "boot". The default bootstrap type for the mean difference is "bca" (bias-corrected & accelerated) as it enjoys the property of being second order accurate and is transformation respecting (see Efron, p. 188).
#' The sampling is done within sample.
#' @importFrom stats
#' @importFrom boot boot
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. Currently, "bootstrap" is the only option.
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "norm", "basic").
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
#' x <- 10:30
#' y <- 1:30
#' ci_median_diff(x, y, R = 999)
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_median_diff <- function(x, y, probs = c(0.025, 0.975), type = "bootstrap",
                           boot_type = c("bca", "perc", "norm", "basic"),
                           R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  stopifnot(length(x) >= 1L,
            length(y) >= 1L)
  estimate <- median(x) - median(y)

  # Calculate CI
  X <- data.frame(v = c(x, y),
                  g = rep(1:2, times = c(length(x), length(y))))
  set_seed(seed)
  S <- boot(X, statistic = function(X, id) boot_two_stats(X, id, FUN = median),
            strata = X[["g"]], R = R, ...)
  cint <- ci_boot(S, boot_type, probs)

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  out <- list(parameter = "population value of median(x)-median(y)",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
