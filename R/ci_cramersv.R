#' CI for the Population Cramer's V
#'
#' This function calculates CIs for the population Cramer's V.
#' By default, a parametric approach based on the non-centrality parameter (NCP)
#' of the chi-squared distribution is utilized.
#' Alternatively, bootstrap CIs are available (by default "bca"),
#' also by boostrapping CIs for the NCP.
#'
#' A positive lower (1-alpha)*100%-confidence limit for the NCP goes hand-in-hand with a
#' significant association test at level alpha. In order to allow such test approach
#' also with Cramer's V, if the lower bound for the NCP is 0,
#' we round down to 0 also the lower bound for Cramer's V.
#' Without this slightly conservative adjustment, the lower limit for V would always be
#' positive since ci for V = sqrt((ci for NCP + df)/(n (k - 1))), where k is the smaller
#' number of levels in the two variables (see Smithson for this formula).
#' Use \code{test_adjustment = FALSE} to switch off this behaviour. Note that this is
#' also a reason to bootstrap V via NCP instead of directly bootstrapping V.
#'
#' Further note that no continuity correction is applied for 2x2 tables,
#' and that large chi-squared test statistics might provide unreliable results with
#' method "chi-squared" (see \code{?pchisq}).
#'
#' @param x The result of \code{stats::chisq.test()}, a matrix/table of counts,
#' or a \code{data.frame} with exactly two columns representing the two variables.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric 95% CI.
#' @param type Type of CI. One of "chi-squared" (default) or "bootstrap".
#' @param boot_type Type of bootstrap CI ("bca", "perc", "norm", "basic").
#' Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param test_adjustment Adjustment to allow for test of association, see Details.
#' The default is \code{TRUE}.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: The parameter in question.
#'   \item \code{interval}: The CI for the parameter.
#'   \item \code{estimate}: The estimate for the parameter.
#'   \item \code{probs}: A vector of error probabilities.
#'   \item \code{type}: The type of the interval.
#'   \item \code{info}: An additional description text for the interval.
#' }
#' @export
#' @examples
#' ci_cramersv(mtcars[c("am", "vs")])
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#' @seealso \code{\link{ci_chisq_ncp}}.
ci_cramersv <- function(x, probs = c(0.025, 0.975), type = c("chi-squared", "bootstrap"),
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999, seed = NULL, test_adjustment = TRUE, ...) {
  # Input check and initialization
  check_probs(probs)
  stopifnot(inherits(x, "htest") || is.matrix(x) || is.data.frame(x))
  if (inherits(x, "htest")) {
    stopifnot("X-squared" %in% names(x[["statistic"]]))
  } else {
    if (is.data.frame(x)) {
      stopifnot(ncol(x) == 2L)
      x <- table(x[, 1L], x[, 2L])
    }
    stopifnot(all(x >= 0))
    x <- stats::chisq.test(x, correct = FALSE)
  }
  stat <- as.numeric(x[["statistic"]])
  n <- sum(x[["observed"]])
  k <- min(dim(x[["observed"]]))
  df <- x[["parameter"]]

  # ci for NCP -> ci for V
  out <- ci_chisq_ncp(
    x,
    probs = probs,
    type = type,
    boot_type = boot_type,
    R = R,
    seed = seed,
    correct = FALSE,
    ...
  )
  ci <- sqrt((out$interval + df) / (n * (k - 1)))

  # Modification to allow hypothesis test of association
  if (test_adjustment && out$interval[1L] == 0) {
    ci[1L] <- 0
  }

  # Replace NCP by Cramer's V
  out$estimate <- cramersv(x)
  out$interval <- check_output(ci, probs = probs, parameter_range = 0:1)
  out$parameter <- "population Cramer's V"
  out
}
