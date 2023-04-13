#' CI for the Population Quantile Difference of two Samples
#'
#' This function calculates bootstrap CIs for the population value of
#' q quantile(x) - q quantile(y), by default using "bca" bootstrap.
#' Resampling is done within sample.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param q A single probability value determining the quantile.
#' Set to 0.5 for the median (default).
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @param type Type of CI. Currently, "bootstrap" is the only option.
#' @param boot_type Type of bootstrap CI ("bca", "perc", "norm", "basic").
#' @param R The number of bootstrap resamples.
#' @param seed An integer random seed.
#' @param ... Further arguments passed to \code{boot::boot()}.
#' @return An object of class "cint" containing these components:
#' \itemize{
#'   \item \code{parameter}: Parameter specification.
#'   \item \code{interval}: CI for the parameter.
#'   \item \code{estimate}: Parameter estimate.
#'   \item \code{probs}: Lower and upper probabilities.
#'   \item \code{type}: Type of interval.
#'   \item \code{info}: Additional description.
#' }
#' @export
#' @examples
#' x <- 10:30
#' y <- 1:30
#' ci_quantile_diff(x, y, R = 999)  # Use larger R
#' @seealso \code{\link{ci_median_diff}}.
ci_quantile_diff <- function(x, y, q = 0.5, probs = c(0.025, 0.975), type = "bootstrap",
                             boot_type = c("bca", "perc", "norm", "basic"),
                             R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)
  stopifnot(length(q) == 1L, q > 0, q < 1)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  stopifnot(
    length(x) >= 1L,
    length(y) >= 1L
  )
  estimate <- stats::quantile(x, probs = q, names = FALSE) -
    stats::quantile(y, probs = q, names = FALSE)

  # Calculate CI
  X <- data.frame(v = c(x, y), g = rep(1:2, times = c(length(x), length(y))))
  check_bca(boot_type, n = nrow(X), R = R)
  set_seed(seed)
  S <- boot::boot(
    X,
    statistic = function(X, id) boot_two_stats(
      X, id, FUN = stats::quantile, probs = q, names = FALSE
    ),
    strata = X[["g"]],
    R = R,
    ...
  )
  cint <- ci_boot(S, boot_type = boot_type, probs = probs)

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(-Inf, Inf))
  out <- list(
    parameter = sprintf(
      "population value of %s quantile(x) - %s quantile(y)", format_p(q), format_p(q)
    ),
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}
