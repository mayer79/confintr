#' Creates Folds
#'
#' This function provides a list of row indices per fold of k-fold cross-validation (basic, stratified, grouped, or blocked). Repeated fold creation is supported as well.
#'
#' By default, the function uses stratified splitting. This will balance the folds regarding the distribution of the input vector \code{y}. Numeric input is first binned into \code{n_bins} quantile groups. If \code{type = "grouped"}, groups specified by \code{y} are kept together when splitting. This is relevant for clustered or panel data. In contrast to basic splitting, \code{type = "blocked"} does not sample indices at random, but rather keeps them in sequential groups.
#' @importFrom MBESS conf.limits.nc.chisq
#' @param y Either the variable used for "stratification" or "grouped" splits. For other types of splits, any vector of the same length as the data intended to split.
#' @param k Number of folds.
#' @param type Split type. One of "stratified", "basic", "grouped", "blocked". The default is "stratified".
#' @param n_bins Approximate numbers of bins for numeric \code{y} and \code{type = "stratified"}.
#' @param m_rep How many times should the data be split into k folds? Default is 1, i.e. no repetitions.
#' @param use_names Should folds be named? Default is \code{TRUE}.
#' @param invert Set to \code{TRUE} if the row numbers not in the fold are to be returned. Default is \code{FALSE}.
#' @param seed Integer random seed.
#' @return A list with row indices per fold.
#' @export
#' @examples
#' y <- rep(c(letters[1:4]), each = 5)
#' create_folds(y)
#' create_folds(y, k = 2)
#' create_folds(y, k = 2, m_rep = 2)
#' create_folds(y, k = 3, type = "blocked")
# Computes approximate one- or two-sided approximate confidence intervals for true Cramér's V
# Package MBESS is required in order to access the non-central chi-squared distribution
#
# Example
#   pw <- iris$Petal.Width > 1
#   ct <- chisq.test(pw, iris$Species)
#   confintCramersV(ct, alternative = "greater") # Estimate 0.91, lower c.i. 0.77
#   confintCramersV(ct, conf.level = 0.9)        # [0.77, 1]
ci_cramersv <- function(x, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {
  alternative <- match.arg(alternative)
  alpha.lower <- alpha.upper <- 0

  if (alternative == "two.sided") {
    alpha.lower <- alpha.upper <- (1 - conf.level) / 2
  } else if (alternative == "greater") {
    alpha.lower <- 1 - conf.level
  } else if (alternative == "less") {
    alpha.upper <- 1 - conf.level
  }

  df <- x$parameter
  chi <- as.numeric(x$statistic)
  n <- sum(x$observed)
  k <- min(dim(x$observed)) - 1
  Delta <- conf.limits.nc.chisq(chi,
                                conf.level = NULL,
                                df = df,
                                alpha.lower = alpha.lower,
                                alpha.upper = alpha.upper)
  Delta <- unlist(Delta[c("Lower.Limit", "Upper.Limit")])
  out <- sqrt(c(Cramers.V = chi, Delta) / (n * k))
  out[is.na(out)] <- 0
  pmin(out, 1)
}
