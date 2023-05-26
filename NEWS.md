# confintr 1.0.2

## Maintenance

- Fix Latex problem in MacOS help files.
- Slight corrections in the documentation.

# confintr 1.0.1

## Maintenance

- Less redundancies in help files
- Using Latex formulas in help files

# confintr 1.0.0

This is a large maintenance update, bumping the package to stable version 1.0.0.

## User visible changes

- Replaced the term "symmetric" by the better "equal-tailed". Similarly, we now output "unequal-tailed" instead of "asymmetric". By "equal-tailed", we mean that the upper and
lower error probabilies agree, not that the interval is symmetric around the estimate. This has no impact on the resulting numbers, only on the text (if you ever used unequal-tailed intervals).

## Maintenance

- Reorganisation of code files
- More compact help files
- Greatly improved unit tests
- Modern code formatting style
- Using `package::function()` notation instead of `importFrom package function`
- Introduction of Github actions
- New [Gitpage](https://mayer79.github.io/confintr/)

# confintr 0.2.0

## Bug fix

Fixes a mistake in the calculation of studentized bootstrap CIs, impacting

- `ci_mean()`, 
- `ci_mean_diff()`, 
- `ci_var()`, 
- `ci_sd()`, and
- `ci_proportion()`

when used together with the options `type = "bootstrap"` **and** `boot_type = "stud"`. The studentized bootstrap is the default `boot_type` for `ci_mean()` and `ci_mean_diff()`.

The mistake happened in calculating the pivotal quantity, not in the statistic itself. Thus, the affected confidence intervals will usually only be slightly off.

### Explanation

"confintr" uses the "boot" package as backend for calculating bootstrap confidence intervals. To calculate studentized confidence bootstrap intervals, `boot()` requires a function that provides two values: the statistic of interest and its *variance*. The "confintr" package passed the *standard deviation* instead of the variance. 

# confintr 0.1.2

This is a maintenance release only, 

- getting rid of the CRAN note on LazyData,
- updating to testthat v3, and
- using a more elegant way to generate/update the package.

# confintr 0.1.1

- Added confidence intervals for the odds ratio via stats::fisher.test.

- Fixed wrong VignetteIndexEntry.

# confintr 0.1.0

This is the initial CRAN release.
