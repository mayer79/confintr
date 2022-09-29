# Helper functions

# Input check
check_probs <- function(probs) {
  stopifnot(length(probs) == 2L,
            is.numeric(probs),
            probs >= 0, probs <= 1,
            probs[1] < probs[2],
            probs[1] + 1 - probs[2] > 0)
  TRUE
}

# Output Check
check_output <- function(ci, probs, parameter_range = c(-Inf, Inf)) {
  stopifnot(length(ci) == 2L,
            length(probs) == 2L,
            length(parameter_range) == 2L)
  ci <- as.numeric(ci)
  w <- which(probs %in% 0:1)
  if (length(w)) {
    ci[w] <- parameter_range[w]
  }
  out <- pmin(pmax(ci, parameter_range[1]), parameter_range[2])
  stopifnot(out[1] <= out[2])
  out
}

# Map boot type to a nice name and also to the output of boot.ci
map_boot_type <- function(ty) {
  out <- c(norm = "normal", basic = "basic", stud = "student",
           perc = "percent", bca = "bca")[ty]
  if (anyNA(out)) {
    stop("Wrong boot_type.")
  }
  as.character(out)
}

# Set seed
set_seed <- function(s) {
  if (!is.null(s)) {
    set.seed(s)
  }
}

# Formats probabilities
format_p <- function(z, digits = max(2L, getOption("digits"))) {
  paste0(format(100 * z, digits = digits), "%")
}

# Pastes together some info on bootstrap
boot_info <- function(type, boot_type, R) {
  if (type == "bootstrap") {
    sprintf("based on %s bootstrap replications and the %s method", R, map_boot_type(boot_type))
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
  FALSE
}

# Title case
title_case1 <- function(s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}

# Map F test statistic to R-squared
f_to_r2 <- function(f, df1, df2) {
  f / (f + df2 / df1)
}

# Map R-squared to F test statistic
#r2_to_f <- function(r2, df1, df2) {
#  r2 / (1 - r2) * df2 / df1
#}

# Map non-centrality parameter of the F distribution to the R-squared
ncp_to_r2 <- function(ncp, df1, df2) {
  ncp / (ncp + df1 + df2 + 1)
}

# Map F test statistic to non-centrality parameter
f_to_ncp <- function(f, df1, df2) {
  df1 * f * (df1 + df2 + 1) / df2
}

# Map chi-squared statistic to non-centrality parameter
chi2_to_ncp <- function(stat, df) {
  pmax(0, stat - df)
}

# Function to efficiently calculate the mean difference statistic in boot
boot_two_means <- function(X, id, se = FALSE, var.equal = FALSE) {
  X <- X[id, ]
  x <- X[X[["g"]] == 1, "v"]
  y <- X[X[["g"]] == 2, "v"]
  c(mean(x) - mean(y), if (se) se_mean_diff(x, y, var.equal = var.equal)^2)
}

# Function to efficiently calculate the median difference statistic in boot
boot_two_stats <- function(X, id, FUN = mean, ...) {
  X <- X[id, ]
  x <- X[X[["g"]] == 1, "v"]
  y <- X[X[["g"]] == 2, "v"]
  FUN(x, ...) - FUN(y, ...)
}

# Error if R < n for bca bootstrap
check_bca <- function(boot_type, n, R) {
  if (boot_type == "bca" && n > R) {
    stop("Number of bootstrap replications must be larger than the sample size.")
  }
  TRUE
}

