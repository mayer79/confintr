#' CI for the Population R-Squared
#'
#' This function calculates parametric CIs for the population \eqn{R^2}.
#' It is based on CIs for the non-centrality parameter \eqn{\Delta} of the F
#' distribution found by test inversion. Values of \eqn{\Delta} are mapped to \eqn{R^2}
#' by \eqn{R^2 = \Delta / (\Delta + \text{df}_1 + \text{df}_2 + 1)},
#' where the \eqn{\text{df}_j} are the degrees of freedom of the F test statistic.
#' A positive lower \eqn{(1 - \alpha) \cdot 100\%}-confidence limit for the \eqn{R^2}
#' goes hand-in-hand with a significant F test at level \eqn{\alpha}.
#'
#' According to [stats::pf()], the results might be unreliable for very large F values.
#' Note that we do not provide bootstrap CIs here to keep the input interface simple.
#'
#' @inheritParams ci_mean
#' @param x The result of [stats::lm()] or the F test statistic.
#' @param df1 The numerator df. Only used if `x` is a test statistic.
#' @param df2 The denominator df. Only used if `x` is a test statistic.
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' fit <- stats::lm(Sepal.Length ~ ., data = iris)
#' summary(fit)$r.squared
#' ci_rsquared(fit)
#' ci_rsquared(fit, probs = c(0.05, 1))
#' @references
#'   Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in
#'     the Social Sciences. New York, NY: Sage Publications.
#' @seealso [ci_f_ncp()]
ci_rsquared <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975)) {
  # Input checks and initialization
  check_probs(probs)
  iprobs <- 1 - probs
  input <- r2_align_input(x, df1 = df1, df2 = df2)
  stat <- input$stat
  df1 <- input$df1
  df2 <- input$df2

  # Calculate limits for ncp
  ncp <- ci_f_ncp(stat, df1 = df1, df2 = df2, probs = probs)[["interval"]]
  cint <- ncp_to_r2(ncp, df1 = df1, df2 = df2)

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(0, 1))
  out <- list(
    parameter = "population R-squared",
    interval = cint,
    estimate = f_to_r2(stat, df1 = df1, df2 = df2),
    probs = probs,
    type = "F",
    info = ""
  )
  class(out) <- "cint"
  out
}

#' CI for the Non-Centrality Parameter of the F Distribution
#'
#' Based on the inversion principle, parametric CIs for the non-centrality parameter
#' (NCP) Delta of the F distribution are calculated.
#' To keep the input interface simple, we do not provide bootstrap CIs here.
#'
#' A positive lower \eqn{(1 - \alpha) \cdot 100\%}-confidence limit for the NCP goes
#' hand-in-hand with a significant F test at level \eqn{\alpha}.
#' According to [stats::pf()], the results might be unreliable for very large F values.
#'
#' @inheritParams ci_rsquared
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' fit <- stats::lm(Sepal.Length ~ ., data = iris)
#' ci_f_ncp(fit)
#' ci_f_ncp(fit, probs = c(0.05, 1))
#' @references
#'   Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the
#'     Social Sciences. New York, NY: Sage Publications.
#' @seealso [ci_rsquared()]
ci_f_ncp <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975)) {
  # Input checks and initialization
  check_probs(probs)
  iprobs <- 1 - probs
  limits <- c(0, Inf)
  input <- r2_align_input(x, df1 = df1, df2 = df2)
  stat <- input$stat
  df1 <- input$df1
  df2 <- input$df2

  # Estimate
  estimate <- f_to_ncp(stat, df1 = df1, df2 = df2)

  # Calculate limits
  if (probs[1L] == 0) {
    lci <- limits[1L]
  } else {
    f1 <- function(ncp) stats::pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[1L]
    lci <- try(stats::uniroot(f1, interval = c(0, estimate))$root, silent = TRUE)
    if (inherits(lci, "try-error")) {
      lci <- limits[1L]
    }
  }
  if (probs[2L] == 1) {
    uci <- limits[2L]
  } else {
    # Upper limit might be improved
    upper_limit <- pmax(4 * estimate, stat * df1 * 4, df1 * 100)
    f2 <- function(ncp) stats::pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[2L]
    uci <- try(
      stats::uniroot(f2, interval = c(estimate, upper_limit))$root, silent = TRUE
    )
    if (inherits(uci, "try-error")) {
      warning("Upper limit outside search range. Set to the maximum of the parameter range.")
      uci <- limits[2L]
    }
  }

  # Organize output
  cint <- check_output(c(lci, uci), probs = probs, parameter_range = limits)
  out <- list(
    parameter = "non-centrality parameter of the F-distribution",
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = "F",
    info = ""
  )
  class(out) <- "cint"
  out
}

# Helper functions

# Combines the two input interfaces
r2_align_input <- function(x, df1, df2) {
  stopifnot(inherits(x, "lm") || is.numeric(x))

  # Distinguish input
  if (inherits(x, "lm")) {
    fstat <- summary(x)[["fstatistic"]]
    stat <- fstat[["value"]]
    df1 <- fstat[["numdf"]]
    df2 <- fstat[["dendf"]]
  } else {
    stopifnot(
      length(x) == 1L,
      !is.null(df1),
      !is.null(df2)
    )
    stat <- x
  }
  return(list(stat = stat, df1 = df1, df2 = df2))
}

# Map F test statistic to non-centrality parameter (Smithson p38)
f_to_ncp <- function(f, df1, df2) {
  df1 * f * (df1 + df2 + 1) / df2
}

# Map F test statistic to R-squared (inverting r2_to_f() below)
f_to_r2 <- function(f, df1, df2) {
  f / (f + df2 / df1)
}

# Map non-centrality parameter of the F distribution to the R-squared
ncp_to_r2 <- function(ncp, df1, df2) {
  ncp / (ncp + df1 + df2 + 1)
}

# Map R-squared to F test statistic (Smithson p38)
#r2_to_f <- function(r2, df1, df2) {
#  r2 / (1 - r2) * df2 / df1
#}
