#' Creates Folds
#'
#' This function provides a list of row indices per fold of k-fold cross-validation (basic, stratified, grouped, or blocked). Repeated fold creation is supported as well.
#'
#' By default, the function uses stratified splitting. This will balance the folds regarding the distribution of the input vector \code{y}. Numeric input is first binned into \code{n_bins} quantile groups. If \code{type = "grouped"}, groups specified by \code{y} are kept together when splitting. This is relevant for clustered or panel data. In contrast to basic splitting, \code{type = "blocked"} does not sample indices at random, but rather keeps them in sequential groups.
#' @importFrom MBESS conf.limits.ncf
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
# fit <- lm(Sepal.Length ~ Petal.Width, data = iris) # R-squared: 0.669
# confintR2(fit, alternative = "greater")            # Lower 95% limit: 0.5982115
confintR2 <- function(x, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {
  alternative <- match.arg(alternative)
  alpha.lower <- alpha.upper <- 0

  if (alternative == "two.sided") {
    alpha.lower <- alpha.upper <- (1 - conf.level) / 2
  } else if (alternative == "greater") {
    alpha.lower <- 1 - conf.level
  } else if (alternative == "less") {
    alpha.upper <- 1 - conf.level
  }

  fstat <- summary(x)$fstatistic
  df.1 <- fstat[2]
  df.2 <- fstat[3]
  Delta <- conf.limits.ncf(F.value = fstat[1],
                           conf.level = NULL,
                           df.1 = df.1,
                           df.2 = df.2,
                           alpha.lower = alpha.lower,
                           alpha.upper = alpha.upper)

  Delta <- unlist(Delta[c("Lower.Limit", "Upper.Limit")])

  out <- c(Delta / (Delta + df.1 + df.2 + 1))
  out[is.na(out)] <- 0
  out
}
