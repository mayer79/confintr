#' Bootstrap Confidence Interval
#'
#' Apply Bootstrap Confidence Interval to result of boot
#'
#' Note that no continuity correction is applied to the 2x2 case.
#' @importFrom boot boot.ci
#' @param S Result of \code{boot}.
#' @param boot_type Confidence interval type passed to \code{boot::boot.ci}.
#' @param probs Probability cuts for the confidence intervals. If one sided or asymmetric, need to be correctly transformed.
#' @param ... Further arguments passed to \code{boot::boot.ci}.
#' @return A numeric vector of length two.
ci_boot <- function(S, boot_type = c("norm", "basic", "stud", "perc", "bca"), probs, ...) {
  a <-  1 - 2 * probs[1]
  b <- -1 + 2 * probs[2]
  boot_type <- match.arg(boot_type)
  conf <- if (probs[1] == 0) b else if (probs[2] == 1) a else c(a, b)
  cint <- boot.ci(S, conf = conf, type = boot_type, ...)[[map_boot_type(boot_type)]]
  m <- ncol(cint)
  if (probs[1] == 0) {
    return(c(-Inf, cint[1, m]))
  } else if (probs[2] == 1) {
    return(c(cint[1, m - 1], Inf))
  }
  c(cint[1, m - 1], cint[2, m])
}
