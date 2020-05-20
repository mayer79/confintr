#' Confidence Interval for the Population Mean
#'
#' This function calculates confidence intervals for the population mean. By default, Student's t method is used. Alternatively, Wald and bootstrap confidence intervals are available. Bootstrap confidence intervals are calculated by the package "resample". The default bootstrap type is "BootstrapT".
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom stats qt qnorm sd
#' @importFrom resample bootstrap
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "t" (default), "Wald", or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param ... Further arguments passed to \code{resample::CI.boot_type}.
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
#' ci_mean(x, type = "Wald")
#' ci_mean(x, type = "bootstrap", R = 1000)
#' ci_mean(x, type = "bootstrap", boot_type = "t", R = 1000)
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
ci_mean <- function(x, probs = c(0.025, 0.975), type = c("t", "Wald", "bootstrap"),
                    boot_type = c("bootstrapT", "percentile", "t", "bca"),
                    R = 10000, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)

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
    if (boot_type == "bootstrapT") {
      S <- bootstrap(x, statistic = c(mean = mean(x), sd = sd(x)), R = R, seed = seed)
    } else {
      S <- bootstrap(x, statistic = mean, R = R, seed = seed)
    }
    cint <- ci_boot(S, boot_type, probs, ...)
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
