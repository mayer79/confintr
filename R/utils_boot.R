#' Bootstrap CI
#'
#' Internal wrapper to \code{boot::boot.ci()}.
#' Contains the logic for also calculating one-sided CIs, and for unequal-tailed CIs.
#' @param S Result of \code{boot::boot()}.
#' @param boot_type CI type passed to \code{boot::boot.ci()}.
#' @param probs Lower and upper probabilities.
#' @return A numeric vector of length two.
ci_boot <- function(S, boot_type = c("norm", "basic", "stud", "perc", "bca"), probs) {
  a <-  1 - 2 * probs[1L]
  b <- -1 + 2 * probs[2L]
  boot_type <- match.arg(boot_type)
  conf <- if (probs[1L] == 0) b else if (probs[2L] == 1) a else c(a, b)
  cint <- boot::boot.ci(S, conf = conf, type = boot_type)[[map_boot_type(boot_type)]]
  cint <- unname(cint)
  m <- ncol(cint)
  if (probs[1L] == 0) {
    return(c(-Inf, cint[1L, m]))
  } else if (probs[2L] == 1) {
    return(c(cint[1L, m - 1L], Inf))
  }
  c(cint[1L, m - 1L], cint[2L, m])
}

# Error if R < n for bca bootstrap
check_bca <- function(boot_type, n, R) {
  if (boot_type == "bca" && n > R) {
    stop("Number of bootstrap replications must be larger than the sample size.")
  }
  TRUE
}

# Map boot type to a nice name and also to the output of boot.ci
map_boot_type <- function(ty) {
  out <- c(
    norm = "normal", basic = "basic", stud = "student", perc = "percent", bca = "bca"
  )[ty]
  if (anyNA(out)) {
    stop("Wrong boot_type.")
  }
  as.character(out)
}

# Pastes together some info on bootstrap
boot_info <- function(type, boot_type, R) {
  if (type == "bootstrap") {
    sprintf(
      "based on %s bootstrap replications and the %s method",
      R,
      map_boot_type(boot_type)
    )
  } else {
    ""
  }
}
