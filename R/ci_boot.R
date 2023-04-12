#' Bootstrap Confidence Interval
#'
#' Internel wrapper to \code{boot::boot.ci}. Contains the logic for one-sided and asymmetric confidence intervals.
#' @importFrom boot boot.ci
#' @param S Result of \code{boot}.
#' @param boot_type Confidence interval type passed to \code{boot::boot.ci}.
#' @param probs Probability cuts for the confidence intervals.
#' @param ... Further arguments passed to \code{boot::boot.ci}.
#' @return A numeric vector of length two.
ci_boot <- function(S, boot_type = c("norm", "basic", "stud", "perc", "bca"),
                    probs, ...) {
  a <-  1 - 2 * probs[1L]
  b <- -1 + 2 * probs[2L]
  boot_type <- match.arg(boot_type)
  conf <- if (probs[1L] == 0) b else if (probs[2L] == 1) a else c(a, b)
  cint <- boot.ci(S, conf = conf, type = boot_type, ...)[[map_boot_type(boot_type)]]
  m <- ncol(cint)
  if (probs[1L] == 0) {
    return(c(-Inf, cint[1L, m]))
  } else if (probs[2L] == 1) {
    return(c(cint[1L, m - 1L], Inf))
  }
  c(cint[1L, m - 1L], cint[2L, m])
}
