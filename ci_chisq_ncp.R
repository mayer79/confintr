#' Confidence Interval for the Non-Centrality Parameter of the Chi-Squared Distribution
#'
#' This function calculates confidence intervals for the non-centrality parameter of the chi-squared distribution based on test inversion.
#'
#' Note that no continuity correction is applied for 2x2 tables. Further note that lower limits below 0.0001 are set to 0 and that for large chi-squared test statisticss might provide unreliable results (see \code{?pchisq}).
#' @importFrom stats chisq.test pchisq optimize
#' @param x The chi-squared test statistic or a \code{data.frame} with exactly two columns.
#' @param df The degrees of freedom. Only used if \code{x} is a test statistic.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "chi-squared" (default) or "bootstrap". "bootstrap" is only supported if \code{x} is a \code{data.frame}.
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
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
#' chisq <- chisq.test(iris$Species, iris$Petal.Width > 1)
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' ci_chisq_ncp(chisq$statistic, df = chisq$parameter)
#' ci_chisq_ncp(ir[, c("Species", "PL")])
#' ci_chisq_ncp(ir[, c("Species", "PL")], probs = c(0.05, 1))
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#' @seealso \code{\link{ci_cramersv}}.
ci_chisq_ncp <- function(x, df = NULL, probs = c(0.025, 0.975),
                         type = c("chi-squared", "bootstrap"),
                         boot_type = c("percentile", "t", "bca"), R = 9999,
                         seed = NULL, ) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)
  iprobs <- 1 - probs
  eps <- 0.0001
  stopifnot(is.data.frame(x) || is.numeric(x))

  # Distinguish input
  if (is.data.frame(x)) {
    stopifnot(ncol(x) == 2L)
  } else {
    stopifnot(length(x) == 1L, !is.null(df))
    stat <- x
  }

  if (type == "chi-squared") {
    # Extract results
    chisq <- chisq.test(x[, 1], x[, 2], correct = FALSE)
    stat <- chisq[["statistic"]]
    df <- chisq[["parameter"]]

    if (probs[1] == 0) {
      lci <- 0
    } else {
      lci <- optimize(function(ncp) (pchisq(stat, df = df, ncp = ncp) - iprobs[1])^2,
                      interval = c(eps / 2, stat))[["minimum"]]
    }
    if (probs[2] == 1) {
      uci <- Inf
    } else {
      uci <- optimize(function(ncp) (pchisq(stat, df = df, ncp = ncp) - iprobs[2])^2,
                      interval = c(stat - df, 4 * stat))[["minimum"]]
    cint <- c(lci, uci)
  }
  } else if (type == "bootstrap") {
    S <- bootstrap(x, statistic = chisq.test(x = x[, 1], y = x[, 2], correct = FALSE)$statistic,
                   R = R, seed = seed)
    cint <- ci_boot(S, boot_type, probs, ...)
  }
  if (cint[1] < eps) {
    cint[1] <- 0
  }

  # Organize output
  cint <- check_output(cint, probs, c(0, Inf))
  out <- list(parameter = "non-centrality parameter of the chi-squared distribution",
              interval = cint, estimate = stat - df,
              probs = probs, type = "chi-squared", info = "")
  class(out) <- "cint"
  out
}
