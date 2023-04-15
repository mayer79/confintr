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
  x <- or_align_input(x)
  x[1L, 1L] / x[2L, 1L] / (x[1L, 2L] / x[2L, 2L])
}

#' CI for the Odds Ratio
#'
#' This function calculates a CI for the odds ratio in a 2x2 table/matrix or a
#' data frame with two columns. The CI is obtained through \code{stats::fisher.test()}.
#' Bootstrap CIs are not available.
#'
#' @param x A 2x2 \code{table/matrix} of frequencies,
#' or a \code{data.frame} with exactly two columns.
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: Parameter specification.
#'   \item \code{interval}: CI for the parameter.
#'   \item \code{estimate}: Parameter estimate.
#'   \item \code{probs}: Lower and upper probabilities.
#'   \item \code{type}: Type of interval.
#'   \item \code{info}: Additional description.
#' }
#' @export
#' @examples
#' x <- cbind(c(10, 5), c(4, 4))
#' ci_oddsratio(x)
#' @seealso \code{\link{oddsratio}}.
ci_oddsratio <- function(x, probs = c(0.025, 0.975)) {
  # Input checks and initialization
  check_probs(probs)
  x <- or_align_input(x)

  # Calculate CI
  cint <- stats::fisher.test(
    x, alternative = probs2alternative(probs), conf.level = diff(probs)
  )$conf.int

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(0, Inf))
  out <- list(
    parameter = "true odds ratio",
    interval = cint,
    estimate = oddsratio(x),
    probs = probs,
    type = "exact",
    info = ""
  )
  class(out) <- "cint"
  out
}

# Helper functions

# Checks input and turns df into table/matrix
or_align_input <- function(x) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  if (is.data.frame(x)) {
    stopifnot(ncol(x) == 2L)
    x <- table(x[, 1L], x[, 2L])
    x <- matrix(x, ncol = ncol(x), dimnames = NULL)
  }
  stopifnot(
    all(x >= 0),
    dim(x) == c(2L, 2L)
  )
  x
}

