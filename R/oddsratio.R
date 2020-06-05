#' Odds Ratio
#'
#' This function calculates the odds ratio in a 2x2 table/matrix or a data frame with two columns. The numerator equals the ratio of the top left entry and the bottom left entry, while the denonimator equals the ratio of the top right entry and the bottom right entry. This is usually slightly different from the calculation by \code{stats::fisher.test} which is based on the ML estimate of the odds ratio.
#' @param x A 2x2 matrix/table of counts or a \code{data.frame} with exactly two columns representing the two binary variables.
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
    x <- table(x[, 1], x[, 2])
  }
  stopifnot(all(x >= 0),
            dim(x) == c(2L, 2L))

  # Odds ratio
  x[1, 1] / x[2, 1] / (x[1, 2] / x[2, 2])
}


