#' Confidence Interval for the Non-Centrality Parameter of the F Distribution
#'
#' Based on the inversion principle, parametric confidence intervals for the non-centrality parameter Delta of the F distribution are calculated. Note that we do not provide bootstrap confidence intervals here to keep the input interface simple. A positive lower (1-alpha)*100%-confidence limit for the ncp goes hand-in-hand with a significant F test at level alpha.
#'
#' Note that, according to \code{?pf}, the results might be unreliable for very large F values.
#' @importFrom stats lm pf uniroot
#' @param x The result of \code{lm} or the F test statistic.
#' @param df1 The numerator degree of freedom, e.g. the number of parameters (including the intercept) of a linear regression. Only used if \code{x} is a test statistic.
#' @param df2 The denominator degree of freedom, e.g. n - df1 - 1 in a linear regression. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
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
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' ci_f_ncp(fit)
#' ci_f_ncp(fit, probs = c(0.05, 1))
#' ci_f_ncp(fit, probs = c(0, 0.95))
#' ci_f_ncp(x = 188.251, df1 = 5, df2 = 144)
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#' @seealso \code{\link{ci_rsquared}}.
ci_f_ncp <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975)) {
  # Input checks and initialization
  check_probs(probs)
  iprobs <- 1 - probs
  limits <- c(0, Inf)
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

  # Estimate
  estimate <- f_to_ncp(stat, df1, df2)

  # Calculate limits
  if (probs[1] == 0) {
    lci <- limits[1]
  } else {
    lci <- try(uniroot(function(ncp) pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[1],
                       interval = c(0, estimate))$root, silent = TRUE)
    if (inherits(lci, "try-error")) {
      lci <- limits[1]
    }
  }
  if (probs[2] == 1) {
    uci <- limits[2]
  } else {
    # Upper limit might be improved
    upper_limit <- pmax(4 * estimate, stat * df1 * 4, df1 * 100)
    uci <- try(uniroot(function(ncp) pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[2],
                       interval = c(estimate, upper_limit))$root,
               silent = TRUE)
    if (inherits(uci, "try-error")) {
      warning("Upper limit outside search range. Set to the maximum of the parameter range.")
      uci <- limits[2]
    }
  }

  # Organize output
  cint <- check_output(c(lci, uci), probs, limits)
  out <- list(parameter = "non-centrality parameter of the F-distribution",
              interval = cint, estimate = estimate,
              probs = probs, type = "F", info = "")
  class(out) <- "cint"
  out
}
