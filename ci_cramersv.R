#' Confidence Interval for the Population Cramer's V
#'
#' This function calculates confidence intervals for the population Cramer's V. By default, a parametric approach based on non-centrality parameter of the chi-squared distribution is utilized. Alternatively, bootstrap confidence intervals are available. Bootstrap confidence intervals are calculated by the package "resample" with default "percentile". Note that bootstrapping Cramer's V is computationally expensive. Further note that no Yate's correction for the 2x2 case is applied when calculating the chi-squared test statistic.
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this. Further note that for large chi-squared test statistics, the results might be unreliable (see \code{?pchisq}).
#' @importFrom stats chisq.test complete.cases
#' @importFrom resample bootstrap
#' @param x A \code{data.frame} with exactly two columns.
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
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' ci_cramersv(ir[, c("Species", "PL")])
#' ci_cramersv(ir[, c("Species", "PL")], type = "bootstrap", R = 1000)
#' ci_cramersv(ir[, c("Species", "PL")], probs = c(0.05, 1))
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#' @seealso \code{\link{ci_chisq_ncp}}.
ci_cramersv <- function(x, probs = c(0.025, 0.975),
                        type = c("chi-squared", "bootstrap"),
                        boot_type = c("percentile", "t", "bca"),
                        R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)
  stopifnot(is.data.frame(x), ncol(x) == 2L)

  # Calculate CI
  if (type == "chi-squared") {
    ncp <- ci_chisq_ncp(x, probs = probs)[["interval"]]

    # Scale to Cramer's V
    chisq <- chisq.test(x[, 1], x[, 2], correct = FALSE)
    n <- sum(chisq[["observed"]])
    k <- min(dim(chisq[["observed"]]))
    df <- chisq[["parameter"]]
    cint <- sqrt((ncp + df) / (n * (k - 1)))
  } else if (type == "bootstrap") {
    S <- bootstrap(x, statistic = cramersv, R = R, seed = seed)
    cint <- ci_boot(S, boot_type, probs, ...)
    if (cint[1] < lower_tol) {
      cint[1] <- 0
    }
  }

  # Organize output
  cint <- check_output(cint, probs, c(0, 1))
  out <- list(parameter = "population Cramér's V",
              interval = cint, estimate = cramersv(x),
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
