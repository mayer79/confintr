#' Confidence Interval for the Population Mean Difference
#'
#' This function calculates confidence intervals for the population value of mean(x) - mean(y). The default is Student's method with Welch's correction for unequal variances, but also bootstrap confidence intervals are available.
#'
#' Bootstrap confidence intervals are calculated by the package "boot". The default bootstrap type for the mean difference is "stud" (bootstrap t) as it enjoys the property of being second order accurate and has a stable variance estimator (see Efron, p. 188).
#' The resampling is done within sample. If \code{boot_type = "stud"}, the standard error is estimated by Welch's method if \code{var.equal = FALSE} (the default) and by pooling otherwise.
#' Thus, \code{var.equal} has not only an effect for the classic Student approach (\code{type = "t"}) but also for \code{boot_type = "stud"}.
#' @importFrom stats t.test
#' @importFrom boot boot
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param var.equal Should the two variances be treated as being equal? The default is \code{FALSE}. If \code{TRUE}, the pooled variance is used to estimate the variance of the mean difference. Otherweise, Welch's approach is used. This also applies to the "stud" boostrap.
#' @param type Type of confidence interval. One of "t" (default), or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("stud", "bca", "perc", "norm", "basic"). Only used for \code{type = "bootstrap"}.
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
#' x <- 10:30
#' y <- 1:30
#' ci_mean_diff(x, y)
#' t.test(x, y)$conf.int
#' ci_mean_diff(x, y, type = "bootstrap", R = 999)
#' @references
#' \enumerate{
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_mean_diff <- function(x, y, probs = c(0.025, 0.975), var.equal = FALSE,
                         type = c("t", "bootstrap"),
                         boot_type = c("stud", "bca", "perc", "norm", "basic"),
                         R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  stopifnot(length(x) >= 1L,
            length(y) >= 1L)
  estimate <- mean(x) - mean(y)

  # Calculate CI
  if (type == "t") {
    cint <- t.test(x, y, var.equal = var.equal,
                   alternative = probs2alternative(probs),
                   conf.level = diff(probs))$conf.int
  } else if (type == "bootstrap") {
    X <- data.frame(v = c(x, y),
                    g = rep(1:2, times = c(length(x), length(y))))
    check_bca(boot_type, nrow(X), R)
    set_seed(seed)
    S <- boot(X, statistic = function(X, id) boot_two_means(X, id,
                se = (boot_type == "stud"), var.equal = var.equal),
              strata = X[["g"]], R = R, ...)
    cint <- ci_boot(S, boot_type, probs)
  }

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  out <- list(parameter = "population value of mean(x)-mean(y)",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
