# confintr

The `confintr` package offers classic and/or bootstrap confidence intervals for the following parameters:

- mean,

- quantiles incl. median,

- proportion,

- variance and standard deviation,

- IQR and MAD,

- skewness and kurtosis,

- R-squared and the non-centrality parameter of the F distribution,

- Cramér's V and the non-centrality parameter of the chi-squared distribution,

- Pearson-, Spearman-, Kendall correlation coefficients,

- mean, quantile and median differences.

Both one- and two-sided intervals are supported.

Different types of bootstrap intervals are possible through argument `boot_type`, see vignette.

## Installation

From CRAN:
``` r
install.packages("confintr")
```

Latest version from github:
``` r
# library(devtools)
install_github("mayer79/confintr")
```

## Teaser

``` r
library(confintr)

# Mean
ci_mean(1:100)
ci_mean(1:100, type = "bootstrap")

# 95% value at risk
ci_quantile(rexp(1000), q = 0.95)

# IQR
ci_IQR(rexp(100), R = 999)

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

# Mean difference
ci_mean_diff(10:30, 1:15)
ci_mean_diff(10:30, 1:15, type = "bootstrap", R  = 999)

# Median difference
ci_median_diff(10:30, 1:15, R  = 999)
```

