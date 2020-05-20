#' Confidence Interval for the Population Variance
#'
#' This function calculates confidence intervals for the population variance. By default classic confidence intervals are calculated based on the chi-squared distribution, assuming normal distribution. Alternatively, bootstrap confidence intervals are supported by the package "resample". The default Bootstrap type is "BootstrapT", utilizing the standard error for the variance given in Wilks, namely the root of (mu4 - (n-3)/(n-1)var^2)/n, where n is the sample size, var the sample variance and mu4 the fouth central moment of the sample. This formula does not assume normality.
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
#' ci_var(x)
#' ci_var(x, type = "bootstrap", R = 1000)
#' ci_var(x, type = "bootstrap", boot_type = "bca", R = 1000)
#' @references
#' \enumerate{
#'   \item Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#'   \item S.S. Wilks (1962), Mathematical Statistics, Wiley & Sons.
#' }
#' @seealso \code{\link{ci_sd}} and \code{\link{stderr_var}}.
ci_var <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                   boot_type = c("bootstrapT", "percentile", "t", "bca"),
                   R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- var(x)
  n <- length(x)

  # Calculate CI
  if (type == "chi-squared") {
    cint <- estimate * (n - 1) / qchisq(1 - probs, df = n - 1)
  } else if (type == "bootstrap") {
    if (boot_type == "bootstrapT") {
      S <- bootstrap(x, statistic = c(var = var(x), sderr = se_var(x)), R = R, seed = seed)
    } else {
      S <- bootstrap(x, statistic = var, R = R, seed = seed)
    }
    cint <- ci_boot(S, boot_type, probs, ...)
  }
  cint <- check_output(cint, probs, c(0, Inf))

  # Organize output
  out <- list(parameter = "population variance",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
