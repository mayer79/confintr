#' Confidence Interval for the Non-Centrality Parameter of the F Distribution
#'
#' This function calculates confidence intervals for the non-centrality parameter of the F distribution based on test inversion. It is based on the function \code{pf}, which - according to \code{?pf} - is not meant to be used with large non-centrality values. Thus, very large F values might lead to problematic results. This function is used mainly to calculate confidence intervals for the population R-squared.
#'
#' Note that for numeric reasons, lower limits below 0.0001 are set to 0.
#' @importFrom stats lm pf optimize
#' @param x The result of \code{lm} or the F test statistic.
#' @param df1 The numerator degree of freedom. Only used if \code{x} is a test statistic.
#' @param df2 The denominator degree of freedom. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: NA.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' ci_f_ncp(fit)
#' ci_f_ncp(x = 188.251, df1 = 5, df2 = 144)
#' @references
#' \enumerate{
#'   \item Kelley, K. (2007). Constructing confidence intervals for standardized effect sizes: Theory, application, and implementation. Journal of Statistical Software, 20 (8), 1–24.
#'   \item Smithson, M. (2003). Confidence intervals. New York, NY: Sage Publications.
#' }
#' @seealso \code{\link{ci_rsquared}}.
ci_f_ncp <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975)) {
  # Input checks and initialization
  check_input(probs)
  dname <- deparse1(substitute(x))
  iprobs <- 1 - probs
  eps <- 0.0001
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

  # Calculate lower limit
  if (probs[1] == 0) {
    lci <- 0
  } else {
    lci <- optimize(function(ncp) (pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[1])^2,
                    interval = c(eps / 2, stat * df1))[["minimum"]]
    if (lci < eps) {
      lci <- 0
    }
  }

  # Calculate upper limit
  if (probs[2] == 1) {
    uci <- Inf
  } else {
    uci <- optimize(function(ncp) (pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[2])^2,
                    interval = c(stat * (df1 - 1), stat * df1 * 4))[["minimum"]]
  }

  # Organize output
  cint <- check_output(c(lci, uci), probs, c(0, Inf))
  prepare_output(cint, estimate = NA, probs = probs, type = "F",
                 boot_type = NA, data_name = dname,
                 estimate_name = "F non-centrality parameter")
}

