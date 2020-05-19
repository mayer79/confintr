#' Cramér's V
#'
#' This function calculates Cramér's V, a measure of association between two categorical variables. It is a scaled version of the chi-squared test statistic and lies between 0 and 1. It is calcuated as sqrt(chi-squared / (n * (k - 1))), where n is the number of observations and k is the smaller of the number of levels of the two variables.
#'
#' @param x A \code{data.frame} with exactly two columns.
#' @param correct Should Yate's continuity correction be applied for the 2x2 case? The default \code{FALSE} gives the usual Cramér's V.
#' @return A numeric vector of length one.
#' @export
#' @examples
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' cramersv(ir[, c("Species", "PL")])
#' @references
#' Cramer, Harald. 1946. Mathematical Methods of Statistics. Princeton: Princeton University Press, page 282 (Chapter 21. The two-dimensional case).
#' @seealso \code{\link{ci_cramersv}}.
cramersv <- function(x, correct = FALSE) {
  # Input check and initialization
  stopifnot(is.data.frame(x),
            ncol(x) == 2L)
  chisq <- chisq.test(x[, 1], x[, 2], correct = correct)
  stat <- as.numeric(chisq[["statistic"]])
  n <- sum(chisq[["observed"]])
  k <- min(dim(chisq[["observed"]]))

  # Cramer's V
  sqrt(stat / (n * (k - 1)))
}


