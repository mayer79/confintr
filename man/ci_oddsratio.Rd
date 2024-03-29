% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci_oddsratio.R
\name{ci_oddsratio}
\alias{ci_oddsratio}
\title{CI for the Odds Ratio}
\usage{
ci_oddsratio(x, probs = c(0.025, 0.975))
}
\arguments{
\item{x}{A 2x2 matrix/table of counts, or a \code{data.frame} with exactly two columns
representing the two binary variables.}

\item{probs}{Lower and upper probabilities, by default \code{c(0.025, 0.975)}.}
}
\value{
An object of class "cint", see \code{\link[=ci_mean]{ci_mean()}} for details.
}
\description{
This function calculates a CI for the odds ratio in a 2x2 table/matrix or a
data frame with two columns. The CI is obtained through \code{\link[stats:fisher.test]{stats::fisher.test()}}.
Bootstrap CIs are not available.
}
\examples{
x <- cbind(c(10, 5), c(4, 4))
ci_oddsratio(x)
}
\seealso{
\code{\link[=oddsratio]{oddsratio()}}.
}
