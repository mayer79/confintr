#' Input check
#'
#' Internal function to check consistency of the two error propabilities.
#' @param probs Error probabilities.
#' @return The result of \code{stopifnot}.
check_input <- function(probs) {
  stopifnot(length(probs) == 2L,
            is.numeric(probs),
            probs >= 0, probs <= 1,
            probs[1] < probs[2],
            probs[1] + 1 - probs[2] > 0)
}
