library(survival)
n <- 100
y <- sample(0:1, n, T)
x <- runif(n)
S <- Surv(rexp(n), sample(0:1, n, TRUE, prob = c(0.1, 0.9)))

conc_measure <- function(x, y, what = "AUC") {

  conc <- survival::concordance(y ~ x)

  if (what == "AUC") {
    stopifnot(all(y == 1 | y == 0))
    out <- conc$concordance
  } else if (what == "SomersD") {
    out <- conc$concordance * 2 - 1
  } else if (what == "HarrellsC") {
    stopifnot(is.Surv(y))
    out <- conc$concordance
  }
  out
}

conc_measure(x, S, "HarrellsC")
Hmisc::rcorr.cens(x, S)

MetricsWeighted::AUC(y, x)

# AUC: y binary and default timewt
# Harrell's c-statistic: y survival
# (Somers' d + 1) / 2: y continuous
# var: somer / 4

