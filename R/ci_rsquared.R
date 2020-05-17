#' Confidence Interval for the Population R-Squared
#'
#' This function calculates parametric confidence intervals for the population R-squared. It is based on confidence intervals for the non-centrality parameter of the F distribution, which is found by test inversion.
#' @importFrom stats lm pf optimize
#' @param x The result of \code{lm} or the F test statistic.
#' @param df1 The first degrees of freedom. Only used if \code{x} is a test statistic.
#' @param df2 The second degrees of freedom. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param lower_tol Values of the lower confidence limit below \code{lower_tol} are considered 0.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: The observed R-squared.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' ci_rsquared(fit)
#' ci_rsquared(188.251, 5, 144)
#' @seealso \code{\link{ci_chisq_ncp}}.
ci_rsquared <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975), lower_tol = 0.0001) {
  # Input checks and initialization
  check_input(probs)
  dname <- deparse1(substitute(x))
  iprobs <- 1 - probs
  stopifnot(inherits(x, "lm") ||
              (is.numeric(x) && length(x) == 1L && !is.null(df1) && !is.null(df2)))

  # Distinguish input
  if (inherits(x, "lm")) {
    sx <- summary(x)
    stopifnot("fstatistic" %in% names(sx))
    fstat <- sx[["fstatistic"]]
    stat <- fstat[["value"]]
    df1 <- fstat[["numdf"]]
    df2 <- fstat[["dendf"]]
  }
  if (is.numeric(x)) {
    stat <- x
  }

  # Calculate limits
  cint <- ci_f_ncp(stat, df1 = df1, df2 = df2, probs = probs, lower_tol = lower_tol)[["conf.int"]]

  # Scale everything to R-squared
  f_to_r2 <- function(f) {
    f / (f + df2 / df1)
  }
  cint <- f_to_r2(cint)

  # Organize output
  cint <- check_output(cint, probs, c(0, 1))
  prepare_output(cint, estimate = f_to_r2(stat), probs = probs, type = "F",
                 boot_type = NA, data_name = dname,
                 estimate_name = "R-squared")
}
