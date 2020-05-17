#' Confidence Interval for the Non-Centrality Parameter of the Chi-Squared Distribution
#'
#' This function calculates confidence intervals for the non-centrality parameter of the chi-squared distribution based on test inversion.
#'
#' Note that no correction is applied for 2x2 tables.
#' @importFrom stats chisq.test pchisq optimize
#' @param The chi-squared test statistic or a \code{data.frame} with exactly two columns.
#' @param df The degrees of freedom. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param search_range Range to be searched for confidence limits. If the lower limit is smaller than twice the minimum, it will be set to 0.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: NA.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
#' @export
#' @examples
#' chisq <- chisq.test(iris$Species, iris$Petal.Width > 1)
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' ci_chisq_ncp(chisq$statistic, df = chisq$parameter)
#' ci_chisq_ncp(ir[, c("Species", "PL")])
#' ci_chisq_ncp(ir[, c("Species", "PL")], probs = c(0.05, 1))
#' @seealso \code{\link{ci_cramersv}}.
ci_chisq_ncp <- function(x, df = NULL, probs = c(0.025, 0.975), search_range = c(0.00001, 1000)) {
  # Input checks and initialization
  check_input(probs)
  dname <- deparse1(substitute(x))
  iprobs <- 1 - probs
  stopifnot(is.data.frame(x) || is.numeric(x))

  # Distinguish input
  if (is.data.frame(x)) {
    stopifnot(ncol(x) == 2L)
    chisq <- chisq.test(x[, 1], x[, 2], correct = FALSE)
    stat <- x[["statistic"]]
    df <- x[["parameter"]]
  } else {
    stopifnot(length(x) == 1L, !is.null(df))
    stat <- x
  }

  # Calculate lower limit
  if (probs[1] == 0) {
    lci <- 0
  } else {
    lci <- optimize(function(ncp) (pchisq(stat, df = df, ncp = ncp) - iprobs[1])^2,
                    interval = search_range)[["minimum"]]
    if (lci < 2 * search_range[1]) {
      lci <- 0
    }
  }

  # Calculate upper limit
  if (probs[2] == 1) {
    uci <- Inf
  } else {
    uci <- optimize(function(ncp) (pchisq(stat, df = df, ncp = ncp) - iprobs[2])^2,
                    interval = search_range)[["minimum"]]
  }

  # Organize output
  cint <- check_output(c(lci, uci), probs, c(0, Inf))
  prepare_output(cint, estimate = NA, probs = probs, type = "chi-squared",
                 boot_type = NA, data_name = dname,
                 estimate_name = "chi-squared non-centrality parameter")
}

