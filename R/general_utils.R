# Helper functions

# Input check
check_probs <- function(probs) {
  stopifnot(
    length(probs) == 2L,
    is.numeric(probs),
    probs >= 0,
    probs <= 1,
    probs[1L] < probs[2L],
    probs[1L] + 1 - probs[2L] > 0
  )
  return(TRUE)
}

# Output Check
check_output <- function(ci, probs, parameter_range = c(-Inf, Inf)) {
  stopifnot(
    length(ci) == 2L,
    length(probs) == 2L,
    length(parameter_range) == 2L,
    all(!is.na(ci))
  )
  ci <- as.numeric(ci)
  w <- which(probs %in% 0:1)
  if (length(w) >= 1L) {
    ci[w] <- parameter_range[w]
  }
  out <- pmin(pmax(ci, parameter_range[1L]), parameter_range[2L])
  stopifnot(out[1] <= out[2])
  out
}

# Sets seed (or not) -> could be placed in a withr::with_seed()
set_seed <- function(s) {
  if (!is.null(s)) {
    set.seed(s)
  }
}

# Formats probabilities
format_p <- function(z, digits = max(2L, getOption("digits"))) {
  paste0(format(100 * z, digits = digits), "%")
}

# Checks if CI is equal-tailed
is_equal_tailed <- function(probs) {
  isTRUE(all.equal(probs[1L], 1 - probs[2L]))
}

# Checks if CI is one-sided
is_onesided <- function(probs) {
  any(probs %in% 0:1)
}

# Calculate prefix from "probs" used in types info
props2text <- function(p) {
  if (is_onesided(p)) {
    out <- "One-sided"
  } else if (!is_equal_tailed(p)) {
    out <- "Unequal-tailed two-sided"
  } else {
    out <- "Two-sided"
  }
  out
}

# Turns probs into alternative hypothesis string
probs2alternative <- function(p) {
  if (is_equal_tailed(p)) {
    return("two.sided")
  } else if (is_onesided(p)) {
    if (p[1L] > 0) {
      return("greater")
    } else {
      return("less")
    }
  }
  unequal_stop()
}

# Consistent error message
unequal_stop <- function() {
  stop("Unequal-tailed two-sided case not supported in this case.")
}

# Title case
title_case1 <- function(s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}
