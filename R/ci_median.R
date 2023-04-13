#' CI for the Population Median
#'
#' This function calculates CIs for the population median by calling
#' \code{ci_quantile(..., q = 0.5)}. See \code{\link{ci_quantile}} for details.
#'
#' @param x A numeric vector.
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @param type Type of CI. One of "binomial" (default), or "bootstrap".
#' @param boot_type Type of bootstrap CI ("bca", "perc", "norm", "basic").
#' Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: Parameter specification.
#'   \item \code{interval}: CI for the parameter.
#'   \item \code{estimate}: Parameter estimate.
#'   \item \code{probs}: Lower and upper probabilities.
#'   \item \code{type}: Type of interval.
#'   \item \code{info}: Additional description.
#' }
#' @export
#' @examples
#' ci_median(1:100)
#' @seealso \code{\link{ci_quantile}}.
ci_median <- function(x, probs = c(0.025, 0.975),
                      type = c("binomial", "bootstrap"),
                      boot_type = c("bca", "perc", "norm", "basic"),
                      R = 9999, seed = NULL, ...) {
  out <- ci_quantile(
    x,
    q = 0.5,
    probs = probs,
    type = type,
    boot_type = boot_type,
    R = R,
    seed = seed,
    ...
  )
  out$parameter <- "population median"
  out
}
