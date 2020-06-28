#' Confidence Interval for Correlation Coefficients
#'
#' This function calculates confidence intervals for a population correlation coefficient. For Pearson correlation, "normal" confidence intervals are available (by \code{stats::cor.test}). Also bootstrap confidence intervals are supported and are the only option for rank correlations.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' @importFrom stats qnorm cor cor.test complete.cases
#' @importFrom boot boot
#' @param x A numeric vector or a \code{matrix/data.frame} with exactly two numeric columns.
#' @param y A numeric vector (only used if \code{x} is a vector).
#' @param method Type of correlation coefficient, one of "pearson" (default), "kendall", or "spearman". For the latter two, only bootstrap confidence intervals are supported. The names can be abbreviated.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "normal" (the default) or "bootstrap" (the only option for rank-correlations).
#' @param boot_type Type of bootstrap confidence interval ("bca", "perc", "norm", "basic"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{boot::boot}.
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
#' ci_cor(iris[1:2])
#' ci_cor(iris[1:2], type = "bootstrap", R = 999, seed = 1)
#' ci_cor(iris[1:2], method = "spearman", type = "bootstrap", R = 999, seed = 1)
#' ci_cor(iris[1:2], method = "k", type = "bootstrap", R = 999, seed = 1)
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_cor <- function(x, y = NULL, probs = c(0.025, 0.975),
                   method = c("pearson", "kendall", "spearman"),
                   type = c("normal", "bootstrap"),
                   boot_type = c("bca", "perc", "norm", "basic"),
                   R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  method <- match.arg(method)
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Distinguish input
  if (is.data.frame(x) || is.matrix(x)) {
    stopifnot(ncol(x) == 2L)
  } else {
    stopifnot(!is.null(y), length(x) == length(y))
    x <- cbind(x, y)
  }

  x <- x[complete.cases(x), ]
  estimate <- cor(x[, 1], x[, 2], method = method)

  # Calculate CI
  if (type == "normal") {
    if (method != "pearson") {
      stop("For rank correlations, only bootstrap confidence intervals are available.")
    }
    cint <- cor.test(x = x[, 1], y = x[, 2], alternative = probs2alternative(probs),
                        conf.level = diff(probs))$conf.int
  } else { # bootstrap
    check_bca(boot_type, nrow(x), R)
    set_seed(seed)
    S <- boot(x, statistic = function(x, id) cor(x[id, 1], x[id, 2], method = method),
              R = R, ...)
    cint <- ci_boot(S, boot_type, probs)
  }

  # Organize output
  cint <- check_output(cint, probs, c(-1, 1))
  out <- list(parameter = sprintf("true %s correlation coefficient", title_case1(method)),
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
