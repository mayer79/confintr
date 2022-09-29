#' Confidence Interval for the Population Mean
#'
#' This function calculates confidence intervals for the population mean. By default, Student's t method is used. Alternatively, Wald and bootstrap confidence intervals are available.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type for the mean is "stud" (bootstrap t) as it enjoys the property of being second order accurate and has a stable variance estimator (see Efron, p. 188).
#' @importFrom stats qt qnorm sd
#' @importFrom boot boot
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "t" (default), "Wald", or "bootstrap".
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
#' x <- 1:100
#' ci_mean(x)
#' ci_mean(x, type = "bootstrap", R = 999, seed = 1)
#' ci_mean(x, type = "bootstrap", R = 999, probs = c(0.025, 1), seed = 1)
#' ci_mean(x, type = "bootstrap", R = 999, probs = c(0, 0.975), seed = 1)
#' @references
#' \enumerate{
#'   \item Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
ci_mean <- function(x, probs = c(0.025, 0.975), type = c("t", "Wald", "bootstrap"),
                    boot_type = c("stud", "bca", "perc", "norm", "basic"),
                    R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- mean(x)

  # Calculate CI
  if (type %in% c("t", "Wald")) {
    q <- if (type == "t") qt(probs, df = length(x) - 1L) else qnorm(probs)
    se <- se_mean(x)
    if (se < 10 * .Machine$double.eps * abs(estimate)) {
      stop("Data essentially constant")
    }
    cint <- estimate + se * q
  } else if (type == "bootstrap") {
    check_bca(boot_type, length(x), R)
    set_seed(seed)
    S <- boot(x, statistic = function(x, id) c(mean(x[id]), se_mean(x[id])^2),
              R = R, ...)
    cint <- ci_boot(S, boot_type, probs)
  }

  # Organize output
  cint <- check_output(cint, probs, c(-Inf, Inf))
  out <- list(parameter = "population mean",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
