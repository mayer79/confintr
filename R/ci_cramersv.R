#' Confidence Interval for the Population Cramer's V
#'
#' This function calculates confidence intervals for the population Cramer's V. By default, a parametric approach based on non-centrality parameter of the chi-squared distribution is utilized. Alternatively, bootstrap confidence intervals are available.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' Note that no continuity correction is applied for 2x2 tables. Further note that very small values are rounded down to 0 and that large chi-squared test statistics might provide unreliable results with method "chi-squared" (see \code{?pchisq}).
#' @importFrom stats chisq.test
#' @param x The result of \code{stats::chisq.test}, a matrix/table of counts or a \code{data.frame} with exactly two columns representing the two variables.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "chi-squared" (default) or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "norm", "basic"). Only used for \code{type = "bootstrap"}.
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
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' ci_cramersv(ir[, c("Species", "PL")])
#' ci_cramersv(ir[, c("Species", "PL")], type = "bootstrap", R = 999)
#' ci_cramersv(ir[, c("Species", "PL")], probs = c(0.05, 1))
#' @references
#' \enumerate{
#'   \item Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
#' @seealso \code{\link{ci_chisq_ncp}}.
ci_cramersv <- function(x, probs = c(0.025, 0.975),
                        type = c("chi-squared", "bootstrap"),
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999, seed = NULL, ...) {
  # Input check and initialization
  check_input(probs)
  stopifnot(inherits(x, "htest") || is.matrix(x) || is.data.frame(x))
  if (inherits(x, "htest")) {
    stopifnot("X-squared" %in% names(x[["statistic"]]))
  } else {
    if (is.data.frame(x)) {
      stopifnot(ncol(x) == 2L)
      x <- table(x[, 1], x[, 2])
    }
    stopifnot(all(x >= 0))
    x <- chisq.test(x, correct = FALSE)
  }
  stat <- as.numeric(x[["statistic"]])
  n <- sum(x[["observed"]])
  k <- min(dim(x[["observed"]]))
  df <- x[["parameter"]]

  # ci for ncp -> ci for V
  out <- ci_chisq_ncp(x, probs = probs, type = type, boot_type = boot_type,
                      R = R, seed = seed, correct = FALSE, ...)
  ci <- sqrt((out$interval + df) / (n * (k - 1)))
  ci <- check_output(ci, probs, 0:1)

  # replace ncp by Cramer's V
  out$estimate <- cramersv(x)
  out$interval <- ci
  out$parameter <- "population Cramer's V"
  out
}