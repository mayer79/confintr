# {confintr} <a href='https://github.com/mayer79/confintr'><img src='man/figures/logo.png' align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/mayer79/confintr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/confintr/actions/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/confintr)](https://cran.r-project.org/package=confintr)
[![Codecov test coverage](https://codecov.io/gh/mayer79/confintr/graph/badge.svg)](https://app.codecov.io/gh/mayer79/confintr)

[![](https://cranlogs.r-pkg.org/badges/confintr)](https://cran.r-project.org/package=confintr) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/confintr?color=orange)](https://cran.r-project.org/package=confintr)

<!-- badges: end -->

## Overview

{confintr} offers classic and/or bootstrap confidence intervals (CI) for the following parameters:

- mean,
- quantiles incl. median,
- proportion,
- variance and standard deviation,
- IQR and MAD,
- skewness and kurtosis,
- R-squared and the non-centrality parameter of the F distribution,
- Cramér's V and the non-centrality parameter of the chi-squared distribution,
- odds ratio of a 2x2 table,
- Pearson-, Spearman-, Kendall correlation coefficients,
- mean differences, quantile and median differences.

Both one- and two-sided intervals are supported.

Different types of bootstrap intervals are available via {boot}, see vignette.

## Installation

```r
# From CRAN
install.packages("confintr")

# Development version
devtools::install_github("mayer79/confintr")
```

## Usage

``` r
library(confintr)
set.seed(1)

# Mean
ci_mean(1:100)

# Two-sided 95% t confidence interval for the population mean
# 
# Sample estimate: 50.5 
# Confidence interval:
#     2.5%    97.5% 
# 44.74349 56.25651 

# Mean using the Bootstrap
ci_mean(1:100, type = "bootstrap")

#   Two-sided 95% bootstrap confidence interval for the population mean
# 	based on 9999 bootstrap replications and the student method
# 
# Sample estimate: 50.5 
# Confidence interval:
#     2.5%    97.5% 
# 44.72913 56.34685

# 95% value at risk
ci_quantile(rexp(1000), q = 0.95)

# 	Two-sided 95% binomial confidence interval for the population 95%
# 	quantile
# 
# Sample estimate: 2.954119 
# Confidence interval:
#     2.5%    97.5% 
# 2.745526 3.499928 

# Mean difference
ci_mean_diff(1:100, 2:101)

#	Two-sided 95% t confidence interval for the population value of mean(x)-mean(y)
#
# Sample estimate: -1 
# Confidence interval:
#      2.5%     97.5% 
# -9.090881  7.090881 

ci_mean_diff(1:100, 2:101, type = "bootstrap", seed = 1)

# Two-sided 95% bootstrap confidence interval for the population value of mean(x)-mean(y)
# based on 9999 bootstrap replications and the student method
#
# Sample estimate: -1 
# Confidence interval:
#      2.5%     97.5% 
# -9.057506  7.092050

# Further examples (without output)

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
ci_mean_diff(10:30, 1:15, type = "bootstrap")

# Median difference
ci_median_diff(10:30, 1:15)
```

