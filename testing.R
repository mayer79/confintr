lapply(list.files("R", full.names = TRUE), source)
# library(cis)

# Old functions
library(MBESS)

confintCramersV <- function(x, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {
  alternative <- match.arg(alternative)
  alpha.lower <- alpha.upper <- 0

  if (alternative == "two.sided") {
    alpha.lower <- alpha.upper <- (1 - conf.level) / 2
  } else if (alternative == "greater") {
    alpha.lower <- 1 - conf.level
  } else if (alternative == "less") {
    alpha.upper <- 1 - conf.level
  }

  df <- x$parameter
  chi <- as.numeric(x$statistic)
  n <- sum(x$observed)
  k <- min(dim(x$observed)) - 1
  Delta <- conf.limits.nc.chisq(chi,
                                conf.level = NULL,
                                df = df,
                                alpha.lower = alpha.lower,
                                alpha.upper = alpha.upper)
  print(Delta)
  Delta <- unlist(Delta[c("Lower.Limit", "Upper.Limit")])
  out <- sqrt(c(Cramers.V = chi, Delta) / (n * k))
  out[is.na(out)] <- 0
  pmin(out, 1)
}

confintR2 <- function(x, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {
  alternative <- match.arg(alternative)
  alpha.lower <- alpha.upper <- 0

  if (alternative == "two.sided") {
    alpha.lower <- alpha.upper <- (1 - conf.level) / 2
  } else if (alternative == "greater") {
    alpha.lower <- 1 - conf.level
  } else if (alternative == "less") {
    alpha.upper <- 1 - conf.level
  }

  fstat <- summary(x)$fstatistic
  df.1 <- fstat[2]
  df.2 <- fstat[3]
  Delta <- conf.limits.ncf(F.value = fstat[1],
                           conf.level = NULL,
                           df.1 = df.1,
                           df.2 = df.2,
                           alpha.lower = alpha.lower,
                           alpha.upper = alpha.upper)
  print(Delta)

  Delta <- unlist(Delta[c("Lower.Limit", "Upper.Limit")])

  out <- c(Delta / (Delta + df.1 + df.2 + 1))
  out[is.na(out)] <- 0
  out
}
