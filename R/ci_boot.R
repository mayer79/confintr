#' Apply Bootstrap Confidence Interval
#'
#' Internal function to calculate a specific type of bootstrap confidence interval from the result of the bootstrap.
#' @importFrom resample CI.bootstrapT CI.percentile CI.t CI.bca jackknife
#' @param S The result of \code{resample::bootstrap}.
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca").
#' @param probs Error probabilities.
#' @param ... Further arguments passed to \code{resample::CI.boot_type}.
#' @return A confidence interval.
ci_boot <- function(S, boot_type = c("bootstrapT", "percentile", "t", "bca"), probs, ...) {
  boot_type <- match.arg(boot_type)
  cint <- switch(boot_type,
                 bootstrapT = CI.bootstrapT(S, probs = probs),
                 percentile = CI.percentile(S, probs = probs, ...),
                 t = CI.t(S, probs = probs),
                 bca = CI.bca(S, probs = probs, ...))
  unname(cint[1, ])
}
