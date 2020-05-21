# cis

The `cis` package offers classic and bootstrap confidence intervals for the following parameters:

- mean,

- quantiles incl. median,

- proportion,

- variance and standard deviation,

- skewness and kurtosis,

- R-squared and the non-centrality parameter of the F distribution,

- Cramér's V and the non-centrality parameter of the chi-squared distribution,

- Pearson-, Spearman-, Kendall correlation coefficients.

Both one- and two-sided intervals are supported.

## Installation

From CRAN:
``` r
install.packages("cis")
```

Latest version from github:
``` r
# library(devtools)
install_github("mayer79/cis")
```

## Teaser

``` r
library(cis)

# Mean
ci_mean(1:100)
ci_mean(1:100, type = "bootstrap")

# 95% value at risk
ci_quantile(rexp(1000), q = 0.95)

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
```

