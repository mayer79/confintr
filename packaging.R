#=====================================================================================
# BUILD THE PACKAGE
#=====================================================================================

if (FALSE) {
  library(resample)
  lapply(list.files("R", full.names = TRUE), source)
}

library(usethis)
library(devtools)

#https://cran.r-project.org/web/packages/QuantileNPCI/QuantileNPCI.pdf

# Create a new package
dir.create(file.path("release"))
pkg <- file.path("release", "cis")

create_package(
  pkg,
  fields = list(
    Title = "Confidence Intervals",
    Type = "Package",
    Version = "0.1.0",
    Date = Sys.Date(),
    Description = "Allows calculation of classic and bootstrap confidence intervals for different parameters such as the population mean, variance, skewness, kurtosis, Cramer's V, R-squared, quantiles, different types of correlaction measures etc.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre'))",
    URL = "https://github.com/mayer79/cis",
    BugReports = "https://github.com/mayer79/cis/issues",
    Depends = "R (>= 3.1.0)",
    VignetteBuilder = "knitr",
    License = "GPL(>= 2)",
    Maintainer = "Michael Mayer <mayermichael79@gmail.com>"))

file.copy(file.path(pkg, "DESCRIPTION"), to = getwd(), overwrite = TRUE)
# Use package has no option to look for pkg, so we first copy description from pkg, modify it and move back
use_package("stats", "Imports")
use_package("resample", "Imports")
use_package("knitr", "Suggests")

# Set up other files -------------------------------------------------
# use_readme_md()
# use_news_md()
# use_cran_comments()

# Copy readme etc.
file.copy(c("NEWS.md", "README.md", "cran-comments.md", "DESCRIPTION", ".Rbuildignore"), pkg, overwrite = TRUE)

# Copy R scripts and document them
if (!dir.exists(file.path(pkg, "R"))) {
  dir.create(file.path(pkg, "R"))
}
file.copy(list.files("R", full.names = TRUE), file.path(pkg, "R"), overwrite = TRUE)
devtools::document(pkg)

if (FALSE) {
  # Copy vignette
  # use_vignette(name = "cis", title = "cis")
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
