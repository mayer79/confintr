#' Cramer's V
#'
#' This function calculates Cramer's V, a measure of association between two categorical variables. It is a scaled version of the chi-squared test statistic and lies between 0 and 1. Cramer's V is calculated as sqrt(chi-squared / (n * (k - 1))), where n is the number of observations and k is the smaller of the number of levels of the two variables.
#'
#' Yates continuity correction is never applied. So in the 2x2 case, if \code{x} is the result of \code{stats::chisq.test}, make sure no continuity correction was applied. Otherwise, results can be inconsistent.
#' @importFrom stats chisq.test
#' @param x The result of \code{stats::chisq.test}, a matrix/table of counts or a \code{data.frame} with exactly two columns representing the two variables.
#' @return A numeric vector of length one.
#' @export
#' @examples
#' tab <- table(mtcars[c("am", "vs")])
#' chi <- chisq.test(tab, correct = FALSE)
#' cramersv(mtcars[c("am", "vs")])
#' cramersv(chi)
#' cramersv(tab)
#' @references
#' Cramer, Harald. 1946. Mathematical Methods of Statistics. Princeton: Princeton University Press, page 282 (Chapter 21. The two-dimensional case).
cramersv <- function(x) {
  # Input check and initialization
  stopifnot(inherits(x, "htest") || is.matrix(x) || is.data.frame(x))
  if (inherits(x, "htest")) {
    stopifnot("X-squared" %in% names(x[["statistic"]]))
  } else {
    if (is.data.frame(x)) {
      stopifnot(ncol(x) == 2L)
      x <- table(x[, 1], x[, 2])
    }
    stopifnot(all(x >= 0))
    x <- chisq.test(x, correct = FALSE)
  }
  stat <- as.numeric(x[["statistic"]])
  n <- sum(x[["observed"]])
  k <- min(dim(x[["observed"]]))

  # Cramer's V
  sqrt(stat / (n * (k - 1)))
}


