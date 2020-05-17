#' Confidence Interval for the Population Standard Deviation
#'
#' This function calculates confidence intervals for the population standard deviation. They are derived by calculating confidence intervals for the variance and then taking the square-root. The classic approach for normally distributed random samples is based on the chi-squared distribution. This is the default. Alternatively, bootstrap confidence intervals are supported by the package "resample".
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom stats var qchisq
#' @importFrom resample bootstrap
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "chisq" (default) or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
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
#' x <- 1:100
#' ci_sd(x)
#' sqrt(ci_var(x)$conf.int)
#' ci_sd(x, type = "bootstrap", R = 1000)
#' @seealso \code{\link{ci_var}}.
ci_sd <- function(x, probs = c(0.025, 0.975), type = c("chisq", "bootstrap"),
                    boot_type = c("bootstrapT", "percentile", "t", "bca"),
                    R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)
  dname <- deparse1(substitute(x))

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- sd(x)
  n <- length(x)

  # Calculate CI
  if (type == "chisq") {
    cint <- estimate * sqrt((n - 1) / qchisq(1 - probs, df = n - 1))
  } else if (type == "bootstrap") {
    if (boot_type == "bootstrapT") {
      S <- bootstrap(x, statistic = c(var = var(x), sderr = stderr_var(x)), R = R, seed = seed)
    } else {
      S <- bootstrap(x, statistic = var, R = R, seed = seed)
    }
    cint <- sqrt(ci_boot(S, boot_type, probs, ...))
  }
  cint <- check_output(cint, probs, c(0, Inf))
  prepare_output(cint, estimate = estimate, probs = probs, type = type,
                 boot_type = boot_type, data_name = dname, estimate_name = "standard deviation")
}
