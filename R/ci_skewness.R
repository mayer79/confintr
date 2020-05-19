#' Confidence Interval for the Skewness
#'
#' This function calculates bootstrap confidence intervals for the population skewness using the R package "resample". The default Bootstrap type is "percentile". "bootstrapT" is not available since there is no simple general formula for the standard error of the skewness.
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom resample bootstrap
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. Currently not used as the only type is "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("percentile", "t", or "bca").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{resample::CI.boot_type}.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: The parameter estimate.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
#' @export
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' ci_skewness(x, R = 1000)
#' @seealso \code{\link{moments}}.
ci_skewness <- function(x, probs = c(0.025, 0.975), type = "bootstrap",
                        boot_type = c("percentile", "t", "bca"),
                        R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)
  dname <- deparse1(substitute(x))

  # Calculate CI
  x <- x[!is.na(x)]
  S <- bootstrap(x, statistic = skewness, R = R, seed = seed)
  cint <- ci_boot(S, boot_type, probs, ...)

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  prepare_output(cint, estimate = skewness(x), probs = probs, type = type,
                 boot_type = boot_type, data_name = dname, estimate_name = "skewness")
}
