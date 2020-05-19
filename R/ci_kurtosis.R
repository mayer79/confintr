#' Confidence Interval for the Kurtosis
#'
#' This function calculates bootstrap confidence intervals for the population kurtosis using the R package "resample". The default Bootstrap type is "percentile". "bootstrapT" is not available since there is no simple general formula for the standard error of the kurtosis.
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' Further note that we use the version of the kurtosis that equals 3 for a normal distribution.
#' @importFrom resample bootstrap
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. Currently not used as the only type is "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("percentile", "t", or "bca").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
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
#' set.seed(1)
#' x <- rnorm(100)
#' ci_kurtosis(x, R = 1000)
#' @seealso \code{\link{moments}}.
ci_kurtosis <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
                        boot_type = c("percentile", "t", "bca"),
                        R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)

  # Calculate CI
  x <- x[!is.na(x)]
  S <- bootstrap(x, statistic = kurtosis, R = R, seed = seed)
  cint <- ci_boot(S, boot_type, probs, ...)

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  out <- list(parameter = "population kurtosis",
              interval = cint, estimate = kurtosis(x),
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
