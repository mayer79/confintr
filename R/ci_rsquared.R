#' CI for the Population R-Squared
#'
#' This function calculates parametric CIs for the population R-squared.
#' It is based on CIs for the non-centrality parameter Delta of the F distribution,
#' found by test inversion. Delta values are mapped to R-squared by
#' R-squared = Delta / (Delta + df1 + df2 + 1), where df1 and df2 are the degrees
#' of freedom (df) of the F test statistic.
#' A positive lower (1-alpha)*100%-confidence limit for the R-squared goes
#' hand-in-hand with a significant F test at level alpha.
#'
#' According to \code{?stats::pf}, the results might be unreliable for very large F values.
#' Note that we do not provide bootstrap CIs here to keep the input interface simple.
#'
#' @param x The result of \code{stats::lm()} or the F test statistic.
#' @param df1 The numerator df. Only used if \code{x} is a test statistic.
#' @param df2 The denominator df. Only used if \code{x} is a test statistic.
#' @param probs Probabilites. The default c(0.025, 0.975) gives a symmetric 95% CI.
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
#' fit <- stats::lm(Sepal.Length ~ ., data = iris)
#' summary(fit)$r.squared
#' ci_rsquared(fit)
#' ci_rsquared(fit, probs = c(0.05, 1))
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#' @seealso \code{\link{ci_f_ncp}}.
ci_rsquared <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975)) {
  # Input checks and initialization
  check_probs(probs)
  iprobs <- 1 - probs
  stopifnot(inherits(x, "lm") || is.numeric(x))

  # Distinguish input
  if (inherits(x, "lm")) {
    fstat <- summary(x)[["fstatistic"]]
    stat <- fstat[["value"]]
    df1 <- fstat[["numdf"]]
    df2 <- fstat[["dendf"]]
  }
  if (is.numeric(x)) {
    stopifnot(
      length(x) == 1L,
      !is.null(df1),
      !is.null(df2)
    )
    stat <- x
  }

  # Calculate limits for ncp
  ncp <- ci_f_ncp(stat, df1 = df1, df2 = df2, probs = probs)[["interval"]]
  cint <- ncp_to_r2(ncp, df1 = df1, df2 = df2)

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(0, 1))
  out <- list(
    interval = cint,
    estimate = f_to_r2(stat, df1 = df1, df2 = df2),
    parameter = "population R-squared",
    probs = probs,
    type = "F",
    info = ""
  )
  class(out) <- "cint"
  out
}
