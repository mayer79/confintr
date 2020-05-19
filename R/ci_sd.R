#' Confidence Interval for the Population Standard Deviation
#'
#' This function calculates confidence intervals for the population standard deviation. They are derived by calculating confidence intervals for the variance and then taking the square-root. The classic approach for normally distributed random samples is based on the chi-squared distribution. This is the default. Alternatively, bootstrap confidence intervals are supported by the package "resample".
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom stats var qchisq
#' @importFrom resample bootstrap
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "chi-squared" (default) or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
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
#' x <- 1:100
#' ci_sd(x)
#' sqrt(ci_var(x)$cint)
#' ci_sd(x, type = "bootstrap", R = 1000)
#' @seealso \code{\link{ci_var}}.
ci_sd <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                    boot_type = c("bootstrapT", "percentile", "t", "bca"),
                    R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- sd(x)
  n <- length(x)

  # Calculate CI
  if (type == "chi-squared") {
    cint <- estimate * sqrt((n - 1) / qchisq(1 - probs, df = n - 1))
  } else if (type == "bootstrap") {
    if (boot_type == "bootstrapT") {
      S <- bootstrap(x, statistic = c(var = var(x), sderr = stderr_var(x)), R = R, seed = seed)
    } else {
      S <- bootstrap(x, statistic = var, R = R, seed = seed)
    }
    cint <- sqrt(ci_boot(S, boot_type, probs, ...))
  }

  # Organize output
  cint <- check_output(cint, probs, c(0, Inf))
  out <- list(parameter = "population standard deviation",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
