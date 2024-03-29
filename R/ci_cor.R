#' CI for Correlation Coefficients
#'
#' This function calculates CIs for a population correlation coefficient.
#' For Pearson correlation, "normal" CIs are available (by [stats::cor.test()]).
#' Also bootstrap CIs are supported (by default "bca", and the only option for
#' rank correlations).
#'
#' @inheritParams ci_mean
#' @param x A numeric vector or a `matrix`/`data.frame` with exactly two numeric columns.
#' @param y A numeric vector (only used if `x` is a vector).
#' @param method Type of correlation coefficient, one of "pearson" (default), "kendall",
#'   or "spearman". For the latter two, only bootstrap CIs are supported.
#' @param type Type of CI. One of "normal" (the default) or "bootstrap"
#'   (the only option for rank-correlations).
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' ci_cor(iris[1:2])
#' ci_cor(iris[1:2], type = "bootstrap", R = 999)  # Use larger R
#' ci_cor(iris[1:2], method = "spearman", type = "bootstrap", R = 999)  # Use larger R
ci_cor <- function(x, y = NULL, probs = c(0.025, 0.975),
                   method = c("pearson", "kendall", "spearman"),
                   type = c("normal", "bootstrap"),
                   boot_type = c("bca", "perc", "norm", "basic"),
                   R = 9999L, seed = NULL, ...) {
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

  x <- x[stats::complete.cases(x), ]
  estimate <- stats::cor(x[, 1L], x[, 2L], method = method)

  # Calculate CI
  if (type == "normal") {
    if (method != "pearson") {
      stop("For rank correlations, only bootstrap CIs are available.")
    }
    cint <- stats::cor.test(
      x = x[, 1L],
      y = x[, 2L],
      alternative = probs2alternative(probs),
      conf.level = diff(probs)
    )$conf.int
  } else { # bootstrap
    check_bca(boot_type, n = nrow(x), R = R)
    set_seed(seed)
    S <- boot::boot(
      x,
      statistic = function(x, id) stats::cor(x[id, 1L], x[id, 2L], method = method),
      R = R,
      ...
    )
    cint <- ci_boot(S, boot_type = boot_type, probs = probs)
  }

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = c(-1, 1))
  out <- list(
    parameter = sprintf("true %s correlation coefficient", title_case1(method)),
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

# Title case
title_case1 <- function(s) {
  paste0(toupper(substring(s, 1L, 1L)), substring(s, 2L))
}
