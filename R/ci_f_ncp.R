#' Confidence Interval for the Non-Centrality Parameter of the F Distribution
#'
#' Based on the inversion principle for monotone distribution functions, parametric confidence intervals for the non-centrality parameter Delta of the F distribution are calculated.
#'
#' Note that for numeric reasons, lower limits below 0.0001 are set to 0. According to \code{?pf}, the results might be unreliable for large F values.
#' @importFrom stats lm pf optimize
#' @param x The result of \code{lm} or the F test statistic.
#' @param df1 The numerator degree of freedom, e.g. the number of parameters (including the intercept) of a linear regression. Only used if \code{x} is a test statistic.
#' @param df2 The denominator degree of freedom, e.g. n - df1 - 1 in a linear regression. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: The estimate of the non-centrality parameter Delta.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' ci_f_ncp(fit)
#' ci_f_ncp(x = 188.251, df1 = 5, df2 = 144)
#' @references Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
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
  prepare_output(cint, estimate = f_to_ncp(stat, df1, df2), probs = probs, type = "F",
                 boot_type = NA, data_name = dname,
                 estimate_name = "F non-centrality parameter")
}

# Helper function
f_to_ncp <- function(f, df1, df2) {
  df1 / df2 * f * (df1 + df2 + 1)
}
