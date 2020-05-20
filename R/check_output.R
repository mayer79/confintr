#' Output Check
#'
#' Internal function to squeeze/expand the resulting confidence interval to the parameter domain.
#' @param ci A confidence interval.
#' @param probs Error probabilities.
#' @param parameter_range Range of parameter domain.
#' @return A confidence interval.
check_output <- function(ci, probs, parameter_range = c(-Inf, Inf)) {
  stopifnot(length(ci) == 2L,
            length(probs) == 2L,
            length(parameter_range) == 2L,
            ci[1] <= ci[2])
  ci <- as.numeric(ci)
  w <- which(probs %in% 0:1)
  if (length(w)) {
    ci[w] <- parameter_range[w]
  }
  pmin(pmax(ci, parameter_range[1]), parameter_range[2])
}



