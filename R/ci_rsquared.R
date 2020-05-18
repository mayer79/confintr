#' Confidence Interval for the Population R-Squared
#'
#' This function calculates parametric confidence intervals for the population R-squared. It is based on confidence intervals for the non-centrality parameter lambda of the F distribution, found by test inversion. The lambda values are mapped to R-squared scale by R-squared = lambda / (lambda + df1 + df2 + 1).
#'
#' According to \code{?pf}, the results might be unreliable for large non-centrality values (resp. in our application large F values). Further note that small values of the lower confidence limits are rounded down to 0.
#' @importFrom stats lm pf optimize
#' @param x The result of \code{lm} or the F test statistic.
#' @param df1 The numerator degree of freedom. Only used if \code{x} is a test statistic.
#' @param df2 The denominator degree of freedom. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
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
#' @references
#' \enumerate{
#'   \item Kelley, K. (2007). Constructing confidence intervals for standardized effect sizes: Theory, application, and implementation. Journal of Statistical Software, 20 (8), 1–24.
#'   \item Smithson, M. (2003). Confidence intervals. New York, NY: Sage Publications.
#' }
#' @seealso \code{\link{ci_f_ncp}}.
ci_rsquared <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975), lower_tol = 0.0001) {
  # Input checks and initialization
  check_input(probs)
  dname <- deparse1(substitute(x))
  iprobs <- 1 - probs
  stopifnot(inherits(x, "lm") || is.numeric(x))

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
    stopifnot(length(x) == 1L,
              !is.null(df1),
              !is.null(df2))
    stat <- x
  }

  # Calculate limits for ncp
  ncp <- ci_f_ncp(stat, df1 = df1, df2 = df2, probs = probs)[["conf.int"]]
  cint <- .ncp_to_r2(ncp, df1, df2)

  # Organize output
  cint <- check_output(cint, probs, c(0, 1))
  prepare_output(cint, estimate = .f_to_r2(stat, df1, df2), probs = probs, type = "F",
                 boot_type = NA, data_name = dname, estimate_name = "R-squared")
}

# Helper functions
.f_to_r2 <- function(f, df1, df2) {
  f / (f + df2 / df1)
}

.ncp_to_r2 <- function(ncp, df1, df2) {
  ncp / (ncp + df1 + df2 + 1)
}
