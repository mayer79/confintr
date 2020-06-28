#' Confidence Interval for the Odds Ratio
#'
#' This function calculates a confidence interval for the odds ratio in a 2x2 table/matrix or a data frame with two columns. The confidence interval is obtained through \code{stats::fisher.test}. Bootstrap confidence intervals are not available.
#' @importFrom stats fisher.test
#' @param x A 2x2 \code{table/matrix} of frequencies, or a \code{data.frame} with exactly two columns.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
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
#' x <- cbind(c(10, 5), c(4, 4))
#' ci_oddsratio(x)
#' @seealso \code{\link{oddsratio}}.
ci_oddsratio <- function(x, probs = c(0.025, 0.975)) {
  # Input checks and initialization
  check_probs(probs)
  stopifnot(is.matrix(x) || is.data.frame(x))

  # Turn input into matrix/table
  if (is.data.frame(x)) {
    stopifnot(ncol(x) == 2L)
    x <- table(x[, 1], x[, 2])
  }
  stopifnot(all(x >= 0),
            dim(x) == c(2L, 2L))

  # Calculate ci
  cint <- fisher.test(x, alternative = probs2alternative(probs),
                      conf.level = diff(probs))$conf.int

  # Organize output
  cint <- check_output(cint, probs, c(0, Inf))
  out <- list(parameter = "true odds ratio",
              interval = cint, estimate = oddsratio(x),
              probs = probs, type = "exact", info = "")
  class(out) <- "cint"
  out
}
