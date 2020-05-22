#' Confidence Interval for the Population Median
#'
#' This function calculates confidence intervals for the population median by calling \code{ci_quantile(..., q = 0.5)}. See \code{\link{ci_quantile}} for details.
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "binomial" (default), or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "norm", "basic"). Only used for \code{type = "bootstrap"}.
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
#' ci_median(1:100)
#' ci_quantile(1:100)
#' @seealso \code{\link{ci_quantile}}.
ci_median <- function(x, probs = c(0.025, 0.975),
                        type = c("binomial", "bootstrap"),
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999, seed = NULL, ...) {
  out <- ci_quantile(x, q = 0.5, probs = probs, type = type,
                     boot_type = boot_type, R = R, seed = seed, ...)
  out$parameter <- "population median"
  out
}
