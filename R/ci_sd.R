#' Confidence Interval for the Population Standard Deviation
#'
#' This function calculates confidence intervals for the population standard deviation. They are derived by calculating confidence intervals for the variance and then taking the square-root. For details, see \code{\link{ci_var}}.
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
#' sd(x)
#' ci_sd(x)
#' sqrt(ci_var(x)$interval)
#' ci_sd(x, type = "bootstrap", R = 999)
#' @seealso \code{\link{ci_var}}.
ci_sd <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                    boot_type = c("bca", "perc", "stud", "norm", "basic"),
                    R = 9999, seed = NULL, ...) {
  out <- ci_var(x = x, probs = probs, type = type, boot_type = boot_type,
                R = R, seed = seed, ...)
  out$estimate <- sqrt(out$estimate)
  out$interval <- sqrt(out$interval)
  out$parameter <- "population standard deviation"
  out
}
