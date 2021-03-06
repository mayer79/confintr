% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci_cor.R
\name{ci_cor}
\alias{ci_cor}
\title{Confidence Interval for Correlation Coefficients}
\usage{
ci_cor(
  x,
  y = NULL,
  probs = c(0.025, 0.975),
  method = c("pearson", "kendall", "spearman"),
  type = c("normal", "bootstrap"),
  boot_type = c("bca", "perc", "norm", "basic"),
  R = 9999,
  seed = NULL,
  ...
)
}
\arguments{
\item{x}{A numeric vector or a \code{matrix/data.frame} with exactly two numeric columns.}

\item{y}{A numeric vector (only used if \code{x} is a vector).}

\item{probs}{Error probabilites. The default c(0.025, 0.975) gives a symmetric 95\% confidence interval.}

\item{method}{Type of correlation coefficient, one of "pearson" (default), "kendall", or "spearman". For the latter two, only bootstrap confidence intervals are supported. The names can be abbreviated.}

\item{type}{Type of confidence interval. One of "normal" (the default) or "bootstrap" (the only option for rank-correlations).}

\item{boot_type}{Type of bootstrap confidence interval ("bca", "perc", "norm", "basic"). Only used for \code{type = "bootstrap"}.}

\item{R}{The number of bootstrap resamples. Only used for \code{type = "bootstrap"}.}

\item{seed}{An integer random seed. Only used for \code{type = "bootstrap"}.}

\item{...}{Further arguments passed to \code{boot::boot}.}
}
\value{
A list with class \code{cint} containing these components:
\itemize{
\item \code{parameter}: The parameter in question.
\item \code{interval}: The confidence interval for the parameter.
\item \code{estimate}: The estimate for the parameter.
\item \code{probs}: A vector of error probabilities.
\item \code{type}: The type of the interval.
\item \code{info}: An additional description text for the interval.
}
}
\description{
This function calculates confidence intervals for a population correlation coefficient. For Pearson correlation, "normal" confidence intervals are available (by \code{stats::cor.test}). Also bootstrap confidence intervals are supported and are the only option for rank correlations.
}
\details{
Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
}
\examples{
ci_cor(iris[1:2])
ci_cor(iris[1:2], type = "bootstrap", R = 999, seed = 1)
ci_cor(iris[1:2], method = "spearman", type = "bootstrap", R = 999, seed = 1)
ci_cor(iris[1:2], method = "k", type = "bootstrap", R = 999, seed = 1)
}
\references{
\enumerate{
\item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
\item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
}
}
