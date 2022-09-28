#' Confidence Interval for the Population Variance
#'
#' This function calculates confidence intervals for the population variance. By default, classic confidence intervals are calculated based on the chi-squared distribution, assuming normal distribution (see Smithson). Bootstrap confidence intervals are also available and are recommended for the non-normal case as the chi-squared confidence intervals are sensitive to deviations from normality.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' The "stud" (bootstrap t) bootstrap uses a general formula for the standard error of the sample variance given in Wilks.
#' @importFrom stats var qchisq
#' @importFrom boot boot
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "chi-squared" (default) or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "stud", "norm", "basic"). Only used for \code{type = "bootstrap"}.
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
#' ci_var(x)
#' ci_var(x, type = "bootstrap", R = 999)
#' @references
#' \enumerate{
#'   \item Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#'   \item S.S. Wilks (1962), Mathematical Statistics, Wiley & Sons.
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
#' @seealso \code{\link{ci_sd}}.
ci_var <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                   boot_type = c("bca", "perc", "stud", "norm", "basic"),
                   R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- var(x)
  n <- length(x)

  # Calculate CI
  if (type == "chi-squared") {
    cint <- estimate * (n - 1) / qchisq(1 - probs, df = n - 1)
  } else if (type == "bootstrap") {
    check_bca(boot_type, n, R)
    set_seed(seed)
    S <- boot(x, statistic = function(x, id) c(var(x[id]), se_var(x[id])^2), R = R, ...)
    cint <- ci_boot(S, boot_type, probs)
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
