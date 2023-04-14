#' CI for the Population Mean Difference
#'
#' This function calculates CIs for the population value of mean(x) - mean(y).
#' The default is Student's method with Welch's correction for unequal variances,
#' but also bootstrap CIs are available. The default bootstrap type is "stud"
#' (bootstrap t) as it has a stable variance estimator (see Efron, p. 188).
#' Resampling is done within sample. If \code{boot_type = "stud"},
#' the standard error is estimated by Welch's method if \code{var.equal = FALSE}
#' (the default), and by pooling otherwise.
#' Thus, \code{var.equal} not only has an effect for the classic Student approach
#' (\code{type = "t"}) but also for \code{boot_type = "stud"}.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @param var.equal Should the two variances be treated as being equal?
#' The default is \code{FALSE}. If \code{TRUE}, the pooled variance is used to estimate
#' the variance of the mean difference. Otherweise, Welch's approach is used.
#' This also applies to the "stud" bootstrap.
#' @param type Type of CI. One of "t" (default), or "bootstrap".
#' @param boot_type Type of bootstrap CI ("stud", "bca", "perc", "norm", "basic").
#' Only used for \code{type = "bootstrap"}.
#' @param R The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.
#' @param seed An integer random seed. Only used for \code{type = "bootstrap"}.
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
#' ci_mean_diff(x, y)
#' t.test(x, y)$conf.int
#' ci_mean_diff(x, y, type = "bootstrap", R = 999)  # Use larger R
#' @references
#' Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
ci_mean_diff <- function(x, y, probs = c(0.025, 0.975), var.equal = FALSE,
                         type = c("t", "bootstrap"),
                         boot_type = c("stud", "bca", "perc", "norm", "basic"),
                         R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)

  # Remove NAs and calculate estimate
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  stopifnot(
    length(x) >= 1L,
    length(y) >= 1L
  )
  estimate <- mean(x) - mean(y)

  # Calculate CI
  if (type == "t") {
    cint <- stats::t.test(
      x,
      y,
      var.equal = var.equal,
      alternative = probs2alternative(probs),
      conf.level = diff(probs)
    )$conf.int
  } else if (type == "bootstrap") {
    X <- data.frame(v = c(x, y), g = rep(1:2, times = c(length(x), length(y))))
    check_bca(boot_type, n = nrow(X), R = R)
    set_seed(seed)
    S <- boot::boot(
      X,
      statistic = function(X, id) boot_two_means(
        X, id, se = (boot_type == "stud"), var.equal = var.equal
      ),
      strata = X[["g"]],
      R = R,
      ...
    )
    cint <- ci_boot(S, boot_type = boot_type, probs = probs)
  }

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(-Inf, Inf))
  out <- list(
    parameter = "population value of mean(x)-mean(y)",
    interval = cint,
    estimate = estimate,
    probs = probs,
    type = type,
    info = boot_info(type, boot_type = boot_type, R = R)
  )
  class(out) <- "cint"
  out
}

#' CI for the Population Median Difference of two Samples
#'
#' This function calculates bootstrap CIs for the population value of
#' median(x) - median(y) by calling ci_quantile_diff(, q = 0.5).
#' See \code{\link{ci_quantile_diff}} for details.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
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
#' ci_median_diff(x, y, R = 999)  # Use larger value for R
#' @seealso \code{\link{ci_quantile_diff}}.
ci_median_diff <- function(x, y, probs = c(0.025, 0.975), type = "bootstrap",
                           boot_type = c("bca", "perc", "norm", "basic"),
                           R = 9999L, seed = NULL, ...) {
  out <- ci_quantile_diff(
    x,
    y,
    q = 0.5,
    probs = probs,
    type = type,
    boot_type = boot_type,
    R = R,
    seed = seed,
    ...
  )
  out$parameter <- "population value of median(x)-median(y)"
  out
}

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
                             R = 9999L, seed = NULL, ...) {
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

# Helper functions

# Function to efficiently calculate the mean difference statistic in boot::boot
boot_two_means <- function(X, id, se = FALSE, var.equal = FALSE) {
  X <- X[id, ]
  x <- X[X[["g"]] == 1, "v"]
  y <- X[X[["g"]] == 2, "v"]
  c(mean(x) - mean(y), if (se) se_mean_diff(x, y, var.equal = var.equal)^2)
}

# Function to efficiently calculate the median difference statistic in boot::boot
boot_two_stats <- function(X, id, FUN = mean, ...) {
  X <- X[id, ]
  x <- X[X[["g"]] == 1, "v"]
  y <- X[X[["g"]] == 2, "v"]
  FUN(x, ...) - FUN(y, ...)
}
