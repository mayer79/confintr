#' Confidence Interval for the Non-Centrality Parameter of the Chi-Squared Distribution
#'
#' This function calculates confidence intervals for the non-centrality parameter of the chi-squared distribution based on chi-squared test inversion or the bootstrap. A positive lower (1-alpha)*100%-confidence limit for the ncp goes hand-in-hand with a significant association test at level alpha.
#'
#' Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
#' Note that large chi-squared test statistics might provide unreliable results with method "chi-squared" (see \code{?pchisq}).
#' @importFrom stats chisq.test pchisq uniroot
#' @importFrom boot boot
#' @param x The result of \code{stats::chisq.test}, a \code{table/matrix} of frequencies, or a \code{data.frame} with exactly two columns.
#' @param probs Error probabilites. The default c(0.025, 0.975) gives a symmetric 95% confidence interval.
#' @param correct Should Yates continuity correction be applied to the 2x2 case? The default is \code{TRUE} (also used in the bootstrap).
#' @param type Type of confidence interval. One of "chi-squared" (default) or "bootstrap".
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
#' ci_chisq_ncp(mtcars[c("am", "vs")])
#' ci_chisq_ncp(mtcars[c("am", "vs")], type = "bootstrap", R = 999)
#' ir <- iris
#' ir$PL <- ir$Petal.Width > 1
#' ci_chisq_ncp(ir[, c("Species", "PL")])
#' ci_chisq_ncp(ir[, c("Species", "PL")], probs = c(0.05, 1))
#' @references
#' \enumerate{
#'   \item Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
#'   \item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
#'   \item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
#' }
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
    x <- chisq.test(x, correct = correct)
  }

  stat <- x[["statistic"]]
  df <- x[["parameter"]]
  estimate <- chi2_to_ncp(stat, df)

  # Calculate ci
  if (type == "chi-squared") {
    iprobs <- 1 - probs
    if (probs[1] == 0) {
      lci <- limits[1]
    } else {
      lci <- try(uniroot(function(ncp) pchisq(stat, df = df, ncp = ncp) - iprobs[1],
                         interval = c(0, estimate))$root, silent = TRUE)
      if (inherits(lci, "try-error")) {
         lci <- limits[1]
      }
    }
    if (probs[2] == 1) {
      uci <- limits[2]
    } else {
      # Upper limit might be improved
      n <- sum(x[["observed"]])
      k <- min(dim(x[["observed"]]))
      upper_limit <- pmax(4 * estimate, df + n * (k - 1), 100)
      uci <- try(uniroot(function(ncp) pchisq(stat, df = df, ncp = ncp) - iprobs[2],
                         interval = c(estimate, upper_limit))$root, silent = TRUE)
      if (inherits(uci, "try-error")) {
        warning("Upper limit outside search range. Set to the maximum of the parameter range.")
        uci <- limits[2]
      }
    }
    cint <- c(lci, uci)
  } else { # bootstrap
    # Reconstruct data
    tab <- data.frame(x[["observed"]])
    X <- tab[rep(seq_len(nrow(tab)), times = tab[, "Freq"]), 1:2]

    # Bootstrap the chi-squared test statistic
    check_bca(boot_type, nrow(X), R)
    set_seed(seed)
    S <- boot(X, statistic = function(x, id) suppressWarnings(chisq.test(
      table(x[id, 1], x[id, 2]), correct = correct)$statistic), R = R, ...)
    cint <- ci_boot(S, boot_type, probs)

    # Map chi-squared to ncp
    cint <- chi2_to_ncp(cint, df)
  }

  # Organize output
  cint <- check_output(cint, probs, limits)
  out <- list(parameter = "non-centrality parameter of the chi-squared distribution",
              interval = cint, estimate = estimate,
              probs = probs, type = type,
              info = boot_info(type, boot_type, R))
  class(out) <- "cint"
  out
}
