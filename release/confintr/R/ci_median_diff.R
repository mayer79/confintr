#' Confidence Interval for the Population Median Difference of two Samples
#'
#' This function calculates bootstrap confidence intervals for the population value of median(x) - median(y) by calling ci_quantile_diff(, q = 0.5). See \code{\link{ci_quantile_diff}} for details.
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
#' @seealso \code{\link{ci_quantile_diff}}.
ci_median_diff <- function(x, y, probs = c(0.025, 0.975), type = "bootstrap",
                           boot_type = c("bca", "perc", "norm", "basic"),
                           R = 9999, seed = NULL, ...) {
  out <- ci_quantile_diff(x, y, q = 0.5, probs = probs, type = type,
                               boot_type = boot_type, R = R, seed = seed, ...)
  out$parameter <- "population value of median(x)-median(y)"
  out
}
