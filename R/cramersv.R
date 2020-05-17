#' Cramer's V
#'
#' This function calculates Cramer's V, a measure of association between two nominal categoricals. It can be viewed as a scaled version of the chi-squared test statistic and lies between 0 and 1. It is calcuated as the square-root of the ratio of the chi-squared test statistic and nk, where n is the sample size and k is min(a, b) - 1. a and b are the number of categories in the two variables.
#'
#' @param x The result of \code{chisq.test} or a \code{data.frame} with exactly two columns.
#' @return A numeric vector of length one.
#' @export
#' @examples
#' chisq <- chisq.test(iris$Species, iris$Petal.Width > 1)
#' cramersv(chisq)
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' cramersv(ir[, c("Species", "PL")])
#' @seealso \code{\link{ci_cramersv}}.
cramersv <- function(x) {
  if (!inherits(x, "htest")) {
    stopifnot(is.data.frame(x), ncol(x) == 2L)
    x <- chisq.test(x[, 1], x[, 2])
  }

  df <- x[["parameter"]]
  stat <- as.numeric(x[["statistic"]])
  n <- sum(x[["observed"]])
  k <- min(dim(x[["observed"]])) - 1

  sqrt(stat / (n * k))
}


