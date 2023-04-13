#' CI for the NCP of the Chi-Squared Distribution
#'
#' This function calculates CIs for the non-centrality parameter (NCP) of the chi-squared
#' distribution. A positive lower (1-alpha)*100%-confidence limit for the NCP goes
#' hand-in-hand with a significant association test at level alpha.
#' By default, CIs are computed by Chi-squared test inversion. This can be unreliable
#' for very large test statistics. The default bootstrap type is "bca".
#'
#' @param x The result of \code{stats::chisq.test()}, a \code{table/matrix} of frequencies,
#' or a \code{data.frame} with exactly two columns.
#' @param probs Lower and upper probabilities, by default c(0.025, 0.975).
#' @param correct Should Yates continuity correction be applied to the 2x2 case?
#' The default is \code{TRUE} (also used in the bootstrap).
#' @param type Type of CI. One of "chi-squared" (default) or "bootstrap".
#' @param boot_type Type of bootstrap CI ("bca", "perc", "norm", "basic").
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
#' ci_chisq_ncp(mtcars[c("am", "vs")])
#' ci_chisq_ncp(mtcars[c("am", "vs")], type = "bootstrap", R = 999)  # Use larger R
#' @references
#' Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#' @seealso \code{\link{ci_cramersv}}.
ci_chisq_ncp <- function(x, probs = c(0.025, 0.975), correct = TRUE,
                         type = c("chi-squared", "bootstrap"),
                         boot_type = c("bca", "perc", "norm", "basic"),
                         R = 9999, seed = NULL, ...) {
  # Input checks and initialization
  type <- match.arg(type)
  boot_type <- match.arg(boot_type)
  check_probs(probs)
  limits <- c(0, Inf)
  stopifnot(inherits(x, "htest") || is.matrix(x) || is.data.frame(x))
  if (inherits(x, "htest")) {
    stopifnot("X-squared" %in% names(x[["statistic"]]))
  }

  # Turn input into stat & df
  if (is.data.frame(x)) {
    stopifnot(ncol(x) == 2L)
    x <- table(x[, 1], x[, 2])
  }
  if (is.matrix(x)) { # a table is a matrix
    stopifnot(all(x >= 0))
    x <- stats::chisq.test(x, correct = correct)
  }

  stat <- x[["statistic"]]
  df <- x[["parameter"]]
  estimate <- chi2_to_ncp(stat, df = df)

  # Calculate ci
  if (type == "chi-squared") {
    iprobs <- 1 - probs
    if (probs[1L] == 0) {
      lci <- limits[1L]
    } else {
      lci <- try(
        stats::uniroot(
          function(ncp) stats::pchisq(stat, df = df, ncp = ncp) - iprobs[1L],
          interval = c(0, estimate)
        )$root,
      silent = TRUE)
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
      uci <- try(
        stats::uniroot(
          function(ncp) stats::pchisq(stat, df = df, ncp = ncp) - iprobs[2L],
          interval = c(estimate, upper_limit)
        )$root,
        silent = TRUE
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
