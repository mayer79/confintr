#' Preparation of Output
#'
#' Internal function to organize output of confidence interval functions.
#'
#' @importFrom stats setNames
#' @param ci A numeric vector of length two representing a confidence interval.
#' @param estimate A numeric vector of length one representing the estimate.
#' @param probs Error probabilites.
#' @param type Type of confidence interval.
#' @param boot_type Type of bootstrap confidence interval.
#' @param data_name Name of the input data.
#' @param estimate_name Name of the estimate.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: The parameter estimate.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
prepare_output <- function(ci, estimate, probs, type, boot_type, data_name, estimate_name) {
  attr(ci, "conf.level") <- probs[2] - probs[1]
  if (any(probs %in% 0:1)) {
    prefx <- "One-sided"
  } else if (!isTRUE(all.equal(probs[1], 1 - probs[2]))) {
    prefx <- "Asymmetric two-sided"
  } else {
    prefx <- "Two-sided"
  }
  suffx <- paste("confidence interval for the population", estimate_name)

  rval <- list(conf.int = ci,
               estimate = setNames(estimate, estimate_name),
               method = paste(prefx, if (type == "bootstrap") sprintf("'%s'", boot_type) else "", type, suffx),
               data.name = data_name)
  class(rval) <- "htest"
  rval
}

