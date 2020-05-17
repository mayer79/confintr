#' Confidence Interval for the Population Cramer's V
#'
#' This function calculates confidence intervals for the population Cramer's V. By default, a parametric approach based on non-centrality parameter of the chis-squared distribution is utilized. Alternatively, bootstrap confidence intervals are available. Bootstrap confidence intervals are calculated by the package "resample" with default "percentile". Note that bootstrapping Cramer's V is computationally expensive. Further note that a significant chi-squared test coincides with a positive lower confidence limit for Cramer's V.
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom stats chisq.test complete.cases
#' @importFrom resample bootstrap
#' @param The result of \code{chisq.test} or a \code{data.frame} with exactly two columns. Type "bootstrap" requires a \code{data.frame}.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "chisq" (default) or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param lower_tol Values of the lower confidence limit below \code{lower_tol} are considered 0.
#' @param ... Further arguments passed to \code{resample::CI.boot_type}.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: The parameter estimate.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
#' @export
#' @examples
#' chisq <- chisq.test(iris$Species, iris$Petal.Width > 1)
#' ci_cramersv(chisq)
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' ci_cramersv(ir[, c("Species", "PL")])
#' ci_cramersv(ir[, c("Species", "PL")], type = "bootstrap", R = 1000)
#' ci_cramersv(chisq, probs = c(0.05, 1))
#' @references
#' Tim Hesterberg (2015). resample: Resampling Functions. R package version 0.4. <CRAN.R-project.org/package=resample>
#' @seealso \code{\link{ci_chisq_ncp}}.
ci_cramersv <- function(x, probs = c(0.025, 0.975), type = c("chisq", "bootstrap"),
                        boot_type = c("percentile", "t", "bca"),
                        R = 10000, seed = NULL, lower_tol = 0.0001, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)
  dname <- deparse1(substitute(x))

  # Distinguish different cases
  if (inherits(x, "htest")) {
    if (type != "chisq") {
      stop("If x is result of chi-squared test, only chi-squared based confidence intervals are available.")
    }
  } else if (is.data.frame(x)) {
    stopifnot(ncol(x) == 2L)
    x <- x[complete.cases(x), , drop = FALSE]
    if (type =="chisq") {
      x <- chisq.test(x[, 1], x[, 2])
    }
  } else {
    stop("Wrong input.")
  }

  # Calculate CI
  if (type == "chisq") {
    cint <- ci_chisq_ncp(x, probs = probs, lower_tol = lower_tol)[["conf.int"]]

    # Scale to Cramer's V
    n <- sum(x[["observed"]])
    k <- min(dim(x[["observed"]])) - 1
    cint <- sqrt(cint / (n * k))
  } else if (type == "bootstrap") {
    S <- bootstrap(x, statistic = cramersv, R = R, seed = seed)
    cint <- ci_boot(S, boot_type, probs, ...)
    if (cint[1] < lower_tol) {
      cint[1] <- 0
    }
  }

  # Organize output
  cint <- check_output(cint, probs, c(0, 1))
  prepare_output(cint, estimate = cramersv(x), probs = probs, type = type,
                 boot_type = boot_type, data_name = dname, estimate_name = "Cramer's V")
}
