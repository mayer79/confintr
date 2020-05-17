#' Confidence Interval for the Non-Centrality Parameter of the F Distribution
#'
#' This function calculates confidence intervals for the non-centrality parameter of the F distribution based on test.
#' @importFrom stats lm pf optimize
#' @param x The result of \code{lm} or the F test statistic.
#' @param df1 The first degrees of freedom. Only used if \code{x} is a test statistic.
#' @param df2 The second degrees of freedom. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param lower_tol Values of the lower confidence limit below \code{lower_tol} are considered 0.
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
#' ci_f_ncp(188.251, 5, 144)
#' @seealso \code{\link{ci_chisq_ncp}}.
ci_f_ncp <- function(x, df1 = NULL, df2 = NULL, probs = c(0.025, 0.975), lower_tol = 0.0001) {
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

  # Calculate lower limit
  if (probs[1] == 0) {
    lci <- 0
  } else {
    lci <- optimize(function(ncp) (pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[1])^2,
                    interval = c(0, stat))[["minimum"]]
    if (lci < lower_tol) {
      lci <- 0
    }
  }

  # Calculate upper limit
  if (probs[2] == 1) {
    uci <- Inf
  } else {
    uci <- optimize(function(ncp) (pf(stat, df1 = df1, df2 = df2, ncp = ncp) - iprobs[2])^2,
                    interval = c(stat, stat * 4))[["minimum"]]
  }

  # Organize output
  cint <- check_output(c(lci, uci), probs, c(0, Inf))
  prepare_output(cint, estimate = NA, probs = probs, type = "F",
                 boot_type = NA, data_name = dname,
                 estimate_name = "F non-centrality parameter")
}

