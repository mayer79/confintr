#' Odds Ratio
#'
#' This function calculates the odds ratio in a 2x2 table/matrix,
#' or a data frame with two columns.
#' The numerator equals the ratio of the top left entry and the bottom left entry of the
#' 2x2 table, while the denominator equals the ratio of the top right entry and the bottom right entry.
#' The result is usually slightly different from the one of \code{stats::fisher.test()},
#' which is based on the ML estimate of the odds ratio.
#' @param x A 2x2 matrix/table of counts, or a \code{data.frame} with exactly two columns
#' representing the two binary variables.
#' @return A numeric vector of length one.
#' @export
#' @examples
#' tab <- cbind(c(10, 5), c(4, 4))
#' oddsratio(tab)
oddsratio <- function(x) {
  # Input check and initialization
  stopifnot(is.matrix(x) || is.data.frame(x))
  if (is.data.frame(x)) {
    stopifnot(ncol(x) == 2L)
    x <- table(x[, 1L], x[, 2L])
  }
  stopifnot(
    all(x >= 0),
    dim(x) == c(2L, 2L)
  )

  # Odds ratio
  x[1L, 1L] / x[2L, 1L] / (x[1L, 2L] / x[2L, 2L])
}


