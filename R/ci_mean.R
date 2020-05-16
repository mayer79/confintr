#' Confidence Interval for the Population Mean
#'
#' This function calculates confidence intervals for the population mean. By default, Student's t method is used. Alternatively, Wald and bootstrap confidence intervals are available. Bootstrap confidence intervals are calculated by the package "resample". The default Bootstrap type is "BootstrapT".
#'
#' Note that for "percentile" and "bca" bootstrap, modified percentiles for better small-sample accuracy are used. Pass \code{expand = FALSE} to \code{...} in order to suppress this.
#' @importFrom stats qt qnorm var sd
#' @importFrom resample bootstrap
#' @param x A numeric vector.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param type Type of confidence interval. One of "t", "Wald", or "bootstrap".
#' @param boot_type Type of bootstrap confidence interval ("bootstrapT", "percentile", "t", or "bca"). Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
#' @param parameter_range Range of parameter of interest (relevant for the one-sided case).
#' @param ... Further arguments passed to \code{resample::CI.boot_type}.
#' @return A list with class \code{htest} containing these components:
#' \itemize{
#'   \item \code{conf.int}: The confidence interval.
#'   \item \code{estimate}: The parameter estimate.
#'   \item \code{method}: A character string describing the applied method.
#'   \item \code{data.name}: A character string with the name(s) of the data.
#' }
#' @export
#' @examples
#' x <- 1:100
#' ci_mean(x)
#' ci_mean(x, type = "Wald")
#' ci_mean(x, type = "bootstrap", R = 1000)
#' @references
#' Tim Hesterberg (2015). resample: Resampling Functions. R package version 0.4. <CRAN.R-project.org/package=resample>
ci_mean <- function(x, probs = c(0.025, 0.975), type = c("t", "Wald", "bootstrap"),
                    boot_type = c("bootstrapT", "percentile", "t", "bca"),
                    R = 10000, seed = NULL, parameter_range = c(-Inf, Inf), ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_input(probs)
  dname <- deparse1(substitute(x))

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  estimate <- mean(x)

  # Calculate CI
  if (type %in% c("t", "Wald")) {
    q <- if (type == "t") qt(probs, df = length(x) - 1L) else qnorm(probs)
    cint <- estimate + .stderr_mean(x) * q
  } else if (type == "bootstrap") {
    if (boot_type == "bootstrapT") {
      S <- bootstrap(x, statistic = c(mean = mean(x), sd = sd(x)), R = R, seed = seed)
    } else {
      S <- bootstrap(x, statistic = mean, R = R, seed = seed)
    }
    cint <- ci_boot(S, boot_type, probs, ...)
  }
  cint <- check_output(cint, probs, parameter_range)
  prepare_output(cint, estimate = estimate, probs = probs, type = type,
                 boot_type = boot_type, data_name = dname, estimate_name = "mean")
}

# Standard error
.stderr_mean <- function(z) {
  stderr <- sqrt(var(z) / length(z))
  if (stderr < 10 * .Machine$double.eps * abs(mean(z))) {
    stop("Data essentially constant")
  }
  stderr
}
