# Helper functions

# Formats probabilities
format_p <- function(z, digits = max(2L, getOption("digits"))) {
  paste0(format(100 * z, digits = digits), "%")
}

# Pastes together some info on bootstrap
boot_info <- function(type, boot_type, R) {
  if (type == "bootstrap") {
    sprintf("based on %s bootstrap samples and the '%s' method", R, boot_type)
  }
}

# Checks if CI is symmetric
is_symmetric <- function(probs) {
  isTRUE(all.equal(probs[1], 1 - probs[2]))
}

# Checks if CI is one-sided
is_onesided <- function(probs) {
  any(probs %in% 0:1)
}

# Calculate prefix from "probs" used in types info
props2text <- function(p) {
  if (is_onesided(p)) {
    out <- "One-sided"
  } else if (!is_symmetric(p)) {
    out <- "Asymmetric two-sided"
  } else {
    out <- "Two-sided"
  }
  out
}

# Turns probs into alternative hypothesis string
probs2alternative <- function(p) {
  if (is_symmetric(p)) {
    return("two.sided")
  } else if (is_onesided(p)) {
    if (p[1] > 0) {
      return("greater")
    } else {
      return("less")
    }
  }
  asymmetric_stop()
}

# Consistent error message
asymmetric_stop <- function() {
  stop("Asymmetric two-sided case not supported in this case.")
}

# Title case
title_case1 <- function(s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}
