#' Confidence Interval for Correlation Coefficients
#'
#' This function calculates confidence intervals for a population correlation coefficients. For Pearson correlation, normal confidence intervals are found by \code{stats::cor.test}. Bootstrap confidence intervals are calculated by the package "resample". The default bootstrap type is "percentile".
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom stats qnorm cor
#' @importFrom resample bootstrap
#' @param x A \code{data.frame} with exactly two numeric columns.
#' @param method Type of correlation coefficient, one of "pearson" (default), "kendall", or "spearman". For the latter two, only bootstrap confidence intervals are supported. The names can be abbreviated.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "normal" (the default), or "bootstrap".
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
#' ci_cor(iris[1:2])
#' ci_cor(iris[1:2], method = "s", type = "bootstrap", R = 1000)
#' @references
#' \enumerate{
#'   \item Clopper, C. and Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika. 26 (4).
#' }
ci_cor <- function(x, probs = c(0.025, 0.975),
                   method = c("pearson", "kendall", "spearman"),
                   type = c("normal", "bootstrap"),
                   boot_type = c("percentile", "t", "bca"),
                   R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  method <- match.arg(method)
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)
  stopifnot(is.data.frame(x), ncol(x) == 2L)
  x <- x[complete.cases(x), ]

  # Calculate CI
  if (type == "normal") {
    if (method != "pearson") {
      stop("For rank correlations, only bootstrap confidence intervals are available.")
    }
    cint <- cor.test(x = x[, 1], y = x[, 2], alternative = probs2alternative(probs),
                        conf.level = diff(probs))$conf.int
  } else { # bootstrap
    S <- bootstrap(x, statistic = function(x) cor(x[, 1], x[, 2], method = method),
                   R = R, seed = seed)
    cint <- ci_boot(S, boot_type, probs, ...)
  }

  # Organize output
  cint <- check_output(cint, probs, c(-1, 1))
  out <- list(parameter = sprintf("true %s correlation coefficient", title_case1(method)),
              interval = cint, estimate = cor(x[, 1], x[, 2], method = method),
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
