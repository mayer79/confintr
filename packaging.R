#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

if (FALSE) {
  library(boot)
  lapply(list.files("R", full.names = TRUE), source)
}

library(usethis)
library(devtools)

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "confintr")

create_package(
  pkg,
  fields = list(
    Title = "Confidence Intervals",
    Type = "Package",
    Version = "0.1.0",
    Date = Sys.Date(),
    Description = "Calculates classic and/or bootstrap confidence intervals for many parameters such as the population mean, variance, IQR, MAD, skewness, kurtosis, Cramer's V, R-squared, quantiles (incl. median), proportions, different types of correlation measures, difference in means, quantiles and medians. Many of the classic confidence intervals are described in Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications. Bootstrap confidence intervals are calculated with the R package 'boot'. Both one- and two-sided intervals are supported.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre'))",
    URL = "https://github.com/mayer79/confintr",
    BugReports = "https://github.com/mayer79/confintr/issues",
    Depends = "R (>= 3.1.0)",
    VignetteBuilder = "knitr",
    License = "GPL(>= 2)",
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>"),
  open = FALSE)

file.copy(file.path(pkg, "DESCRIPTION"), to = getwd(), overwrite = TRUE)
# Use package has no option to look for pkg, so we first copy description from pkg, modify it and move back
use_package("stats", "Imports")
use_package("boot", "Imports")
use_package("knitr", "Suggests")
use_package("testthat", "Suggests")

# Set up other files -------------------------------------------------
# use_readme_md()
# use_news_md()
# use_cran_comments()
# use_testthat()

# Copy readme etc.
file.copy(c("NEWS.md", "README.md", "cran-comments.md", "DESCRIPTION", ".Rbuildignore"),
          pkg, overwrite = TRUE)

# Copy R scripts and document them
if (!dir.exists(file.path(pkg, "R"))) {
  dir.create(file.path(pkg, "R"))
}
file.copy(list.files("R", full.names = TRUE), file.path(pkg, "R"), overwrite = TRUE)
devtools::document(pkg)

# Tests
if (!dir.exists(file.path(pkg, "tests"))) {
  dir.create(file.path(pkg, "tests"))
}
file.copy("tests", pkg, recursive = TRUE)
test(pkg)

if (TRUE) {
  # Copy vignette
  # use_vignette(name = "confintr", title = "confintr")
  dir.create(file.path(pkg, "vignettes"))
  dir.create(file.path(pkg, "doc"))
  dir.create(file.path(pkg, "Meta"))
  file.copy(list.files("vignettes", full.names = TRUE),
            file.path(pkg, "vignettes"), overwrite = TRUE)

  devtools::build_vignettes(pkg)
}

# Check
check(pkg, manual = TRUE)

# Create
build(pkg)

# Install
install(pkg)

check_win_devel(pkg)

check_rhub(pkg)

devtools::release(pkg)
