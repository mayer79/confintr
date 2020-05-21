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

set.seed(100)
x <- rnorm(100)
y <- x + rnorm(100)
(cor.test(~x+y)$conf.int)^2
fit <- lm(y~x)
ci_rsquared(fit)
confintR2(fit)
unlist(ci.R2(0.5726243, 1, 98, Random.Predictors = FALSE)[c(1, 3)])



# CRAMERS V
chisq <- chisq.test(iris$Species, iris$Petal.Width > 1)
cramersv(chisq)
ci_cramersv(chisq)
confintCramersV(chisq)
ci_cramersv(chisq, probs = c(0.2, 0.8))
confintCramersV(chisq, conf.level = 0.6)

ir <- iris
ir$PL <- ir$Petal.Width > 1
cramersv(ir[, c("Species", "PL")])
ci_cramersv(ir[, c("Species", "PL")])
ci_cramersv(ir[, c("Species", "PL")], type = "bootstrap", R = 1000)



# Mean
ci_mean(1:100)
ci_mean(1:100, type = "bootstrap")

# Correlation
ci_cor(iris[1:2], method = "spearman", type = "bootstrap")

# Proportions
ci_proportion(10, n = 100, type = "Wilson")
ci_proportion(10, n = 100, type = "Clopper-Pearson")

# R-squared
fit <- lm(Sepal.Length ~ ., data = iris)
ci_rsquared(fit, probs = c(0.05, 1))

# Kurtosis
ci_kurtosis(1:100)


# Der folgende Code berechnet ein 95\%-Konfidenzinterval
k <- qbinom(0.025, 76, 0.5)
ell <- qbinom(0.975, 76, 0.5) + 1
sort(wohnungen$Preis)[c(k, ell)]    # Ergibt 1093 1270
