% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci_median_diff.R
\name{ci_median_diff}
\alias{ci_median_diff}
\title{Confidence Interval for the Population Median Difference of two Samples}
\usage{
ci_median_diff(
  x,
  y,
  probs = c(0.025, 0.975),
  type = "bootstrap",
  boot_type = c("bca", "perc", "norm", "basic"),
  R = 9999,
  seed = NULL,
  ...
)
}
\arguments{
\item{x}{A numeric vector.}

\item{y}{A numeric vector.}

\item{probs}{Error probabilites. The default c(0.025, 0.975) gives a symmetric 95\% confidence interval.}

\item{type}{Type of confidence interval. Currently, "bootstrap" is the only option.}

\item{boot_type}{Type of bootstrap confidence interval ("bca", "perc", "norm", "basic").}

\item{R}{The number of bootstrap resamples.}

\item{seed}{An integer random seed.}

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
This function calculates bootstrap confidence intervals for the population value of median(x) - median(y) by calling ci_quantile_diff(, q = 0.5). See \code{\link{ci_quantile_diff}} for details.
}
\examples{
x <- 10:30
y <- 1:30
ci_median_diff(x, y, R = 999)
}
\seealso{
\code{\link{ci_quantile_diff}}.
}
