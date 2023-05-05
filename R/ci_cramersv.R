#' Cramer's V
#'
#' This function calculates Cramer's V, a measure of association between two categorical
#' variables.
#'
#' Cramer's V is a scaled version of the chi-squared test statistic and takes values in
#' the unit interval. It is calculated as \eqn{\sqrt{\chi^2 / (n * (k - 1))}},
#' where \eqn{n} is the number of observations, and \eqn{k} is the smaller of the number
#' of levels of the two variables.
#'
#' Yates continuity correction is never applied. So in the 2x2 case, if `x` is the
#' result of [stats::chisq.test()], make sure no continuity correction was applied.
#' Otherwise, results can be inconsistent.
#'
#' @param x The result of [stats::chisq.test()], a matrix/table of counts, or
#'   a `data.frame` with exactly two columns representing the two variables.
#' @returns A numeric vector of length one.
#' @export
#' @examples
#' cramersv(mtcars[c("am", "vs")])
#' @references
#' Cramer, Harald. 1946. Mathematical Methods of Statistics. Princeton: Princeton University Press, page 282 (Chapter 21. The two-dimensional case).
cramersv <- function(x) {
  x <- cramersv_align_input(x, correct = FALSE)
  stat <- as.numeric(x[["statistic"]])
  n <- sum(x[["observed"]])
  k <- min(dim(x[["observed"]]))
  sqrt(stat / (n * (k - 1)))
}

#' CI for the Population Cramer's V
#'
#' This function calculates CIs for the population Cramer's V.
#' By default, a parametric approach based on the non-centrality parameter (NCP)
#' of the chi-squared distribution is utilized.
#' Alternatively, bootstrap CIs are available (default "bca"),
#' also by boostrapping CIs for the NCP and then mapping the result back to Cramer's V.
#'
#' A positive lower \eqn{(1 - \alpha) \cdot 100\%}-confidence limit for the NCP goes
#' hand-in-hand with a significant association test at level alpha. In order to allow
#' such test approach also with Cramer's V, if the lower bound for the NCP is 0,
#' we round down to 0 the lower bound for Cramer's V as well.
#' Without this slightly conservative adjustment, the lower limit for V would always be
#' positive since the CI for \eqn{V = \sqrt{(\text{CI for NCP} + df)/(n (k - 1))}},
#' where \eqn{k} is the smaller number of levels in the two variables
#' (see Smithson, p40).
#' Use `test_adjustment = FALSE` to switch off this behaviour. Note that this is
#' also a reason to bootstrap V via NCP instead of directly bootstrapping V.
#'
#' Further note that no continuity correction is applied for 2x2 tables,
#' and that large chi-squared test statistics might provide unreliable results with
#' method "chi-squared", see [stats::pchisq()].
#'
#' @inheritParams cramersv
#' @inheritParams ci_mean
#' @param type Type of CI. One of "chi-squared" (default) or "bootstrap".
#' @param test_adjustment Adjustment to allow for test of association, see Details.
#'   The default is `TRUE`.
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' # Example from Smithson, M., page 41
#' test_scores <- as.table(
#'   rbind(
#'     Private = c(6, 14, 17, 9),
#'     Public = c(30, 32, 17, 3)
#'   )
#' )
#' suppressWarnings(X2 <- stats::chisq.test(test_scores))
#' ci_cramersv(X2)
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#' @seealso \code{\link{ci_chisq_ncp}}.
ci_cramersv <- function(x, probs = c(0.025, 0.975),
                        type = c("chi-squared", "bootstrap"),
                        boot_type = c("bca", "perc", "norm", "basic"),
                        R = 9999L, seed = NULL, test_adjustment = TRUE, ...) {
  # Input check and initialization
  check_probs(probs)
  x <- cramersv_align_input(x, correct = FALSE)
  stat <- as.numeric(x[["statistic"]])
  n <- sum(x[["observed"]])
  k <- min(dim(x[["observed"]]))
  df <- x[["parameter"]]

  # ci for NCP -> ci for V
  out <- ci_chisq_ncp(
    x,
    probs = probs,
    type = type,
    boot_type = boot_type,
    R = R,
    seed = seed,
    correct = FALSE,
    ...
  )
  ci <- sqrt((out$interval + df) / (n * (k - 1)))  # Smithson p40

  # Modification to allow hypothesis test of association
  if (test_adjustment && out$interval[1L] == 0) {
    ci[1L] <- 0
  }

  # Replace NCP by Cramer's V
  out$estimate <- cramersv(x)
  out$interval <- check_output(ci, probs = probs, parameter_range = 0:1)
  out$parameter <- "population Cramer's V"
  out
}

#' CI for the NCP of the Chi-Squared Distribution
#'
#' This function calculates CIs for the non-centrality parameter (NCP) of the chi-squared
#' distribution. A positive lower \eqn{(1 - \alpha) \cdot 100\%}-confidence limit
#' for the NCP goes hand-in-hand with a significant association test at level alpha.
#'
#' By default, CIs are computed by Chi-squared test inversion. This can be unreliable
#' for very large test statistics. The default bootstrap type is "bca".
#'
#' @inheritParams ci_cramersv
#' @inheritParams cramersv
#' @inheritParams ci_mean
#' @param correct Should Yates continuity correction be applied to the 2x2 case? The
#'   default is `TRUE` (also used in the bootstrap), which differs from [ci_cramersv()].
#' @returns An object of class "cint", see [ci_mean()] for details.
#' @export
#' @examples
#' ci_chisq_ncp(mtcars[c("am", "vs")])
#' ci_chisq_ncp(mtcars[c("am", "vs")], type = "bootstrap", R = 999)  # Use larger R
#' @references
#'   Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the
#'     Social Sciences. New York, NY: Sage Publications.
#' @seealso [ci_cramersv()]
ci_chisq_ncp <- function(x, probs = c(0.025, 0.975), correct = TRUE,
                         type = c("chi-squared", "bootstrap"),
                         boot_type = c("bca", "perc", "norm", "basic"),
                         R = 9999L, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)
  limits <- c(0, Inf)
  x <- cramersv_align_input(x, correct = correct)
  stat <- x[["statistic"]]
  df <- x[["parameter"]]
  estimate <- chi2_to_ncp(stat, df = df)

  # Calculate ci
  if (type == "chi-squared") {
    iprobs <- 1 - probs
    if (probs[1L] == 0) {
      lci <- limits[1L]
    } else {
      f1 <- function(ncp) stats::pchisq(stat, df = df, ncp = ncp) - iprobs[1L]
      lci <- try(stats::uniroot(f1, interval = c(0, estimate))$root, silent = TRUE)
      if (inherits(lci, "try-error")) {
        lci <- limits[1L]
      }
    }
    if (probs[2L] == 1) {
      uci <- limits[2L]
    } else {
      # Upper limit might be improved
      n <- sum(x[["observed"]])
      k <- min(dim(x[["observed"]]))
      upper_limit <- pmax(4 * estimate, df + n * (k - 1), 100)
      f2 <- function(ncp) stats::pchisq(stat, df = df, ncp = ncp) - iprobs[2L]
      uci <- try(
        stats::uniroot(f2, interval = c(estimate, upper_limit))$root, silent = TRUE
      )
      if (inherits(uci, "try-error")) {
        warning("Upper limit outside search range. Set to the maximum of the parameter range.")
        uci <- limits[2L]
      }
    }
    cint <- c(lci, uci)
  } else { # bootstrap
    # Reconstruct data
    tab <- data.frame(x[["observed"]])
    X <- tab[rep(seq_len(nrow(tab)), times = tab[, "Freq"]), 1:2]

    # Bootstrap the chi-squared test statistic
    check_bca(boot_type, n = nrow(X), R = R)
    set_seed(seed)
    S <- boot::boot(
      X,
      statistic = function(x, id) suppressWarnings(
        stats::chisq.test(table(x[id, 1L], x[id, 2L]), correct = correct)$statistic
      ),
      R = R,
      ...
    )
    cint <- ci_boot(S, boot_type = boot_type, probs = probs)

    # Map chi-squared to ncp
    cint <- chi2_to_ncp(cint, df = df)
  }

  # Organize output
  cint <- check_output(cint, probs = probs, parameter_range = limits)
  out <- list(
    parameter = "non-centrality parameter of the chi-squared distribution",
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

# Map chi-squared statistic to non-centrality parameter
chi2_to_ncp <- function(stat, df) {
  pmax(0, stat - df)
}

# Checks input and turns df into table/matrix
cramersv_align_input <- function(x, correct = FALSE) {
  stopifnot(inherits(x, "htest") || is.matrix(x) || is.data.frame(x))
  if (inherits(x, "htest")) {
    stopifnot("X-squared" %in% names(x[["statistic"]]))
  } else {
    if (is.data.frame(x)) {
      stopifnot(ncol(x) == 2L)
      x <- table(x[, 1L], x[, 2L])
    }
    stopifnot(all(x >= 0))
    x <- stats::chisq.test(x, correct = correct)
  }
  return(x)
}
