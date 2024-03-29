% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci_cor.R
\name{ci_cor}
\alias{ci_cor}
\title{CI for Correlation Coefficients}
\usage{
ci_cor(
  x,
  y = NULL,
  probs = c(0.025, 0.975),
  method = c("pearson", "kendall", "spearman"),
  type = c("normal", "bootstrap"),
  boot_type = c("bca", "perc", "norm", "basic"),
  R = 9999L,
  seed = NULL,
  ...
)
}
\arguments{
\item{x}{A numeric vector or a \code{matrix}/\code{data.frame} with exactly two numeric columns.}

\item{y}{A numeric vector (only used if \code{x} is a vector).}

\item{probs}{Lower and upper probabilities, by default \code{c(0.025, 0.975)}.}

\item{method}{Type of correlation coefficient, one of "pearson" (default), "kendall",
or "spearman". For the latter two, only bootstrap CIs are supported.}

\item{type}{Type of CI. One of "normal" (the default) or "bootstrap"
(the only option for rank-correlations).}

\item{boot_type}{Type of bootstrap CI. Only used for \code{type = "bootstrap"}.}

\item{R}{The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.}

\item{seed}{An integer random seed. Only used for \code{type = "bootstrap"}.}

\item{...}{Further arguments passed to \code{\link[boot:boot]{boot::boot()}}.}
}
\value{
An object of class "cint", see \code{\link[=ci_mean]{ci_mean()}} for details.
}
\description{
This function calculates CIs for a population correlation coefficient.
For Pearson correlation, "normal" CIs are available (by \code{\link[stats:cor.test]{stats::cor.test()}}).
Also bootstrap CIs are supported (by default "bca", and the only option for
rank correlations).
}
\examples{
ci_cor(iris[1:2])
ci_cor(iris[1:2], type = "bootstrap", R = 999)  # Use larger R
ci_cor(iris[1:2], method = "spearman", type = "bootstrap", R = 999)  # Use larger R
}
