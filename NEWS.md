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
