% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci_chisq_ncp.R
\name{ci_chisq_ncp}
\alias{ci_chisq_ncp}
\title{Confidence Interval for the Non-Centrality Parameter of the Chi-Squared Distribution}
\usage{
ci_chisq_ncp(
  x,
  probs = c(0.025, 0.975),
  correct = TRUE,
  type = c("chi-squared", "bootstrap"),
  boot_type = c("bca", "perc", "norm", "basic"),
  R = 9999,
  seed = NULL,
  ...
)
}
\arguments{
\item{x}{The result of \code{stats::chisq.test}, a \code{table/matrix} of frequencies, or a \code{data.frame} with exactly two columns.}

\item{probs}{Error probabilites. The default c(0.025, 0.975) gives a symmetric 95\% confidence interval.}

\item{correct}{Should Yates continuity correction be applied to the 2x2 case? The default is \code{TRUE} (also used in the bootstrap).}

\item{type}{Type of confidence interval. One of "chi-squared" (default) or "bootstrap".}

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
This function calculates confidence intervals for the non-centrality parameter of the chi-squared distribution based on chi-squared test inversion or the bootstrap. A positive lower (1-alpha)*100\%-confidence limit for the ncp goes hand-in-hand with a significant association test at level alpha.
}
\details{
Bootstrap confidence intervals are calculated by the package "boot", see references. The default bootstrap type is "bca" (bias-corrected accelerated) as it enjoys the property of being second order accurate as well as transformation respecting (see Efron, p. 188).
Note that large chi-squared test statistics might provide unreliable results with method "chi-squared" (see \code{?pchisq}).
}
\examples{
ci_chisq_ncp(mtcars[c("am", "vs")])
ci_chisq_ncp(mtcars[c("am", "vs")], type = "bootstrap", R = 999)
ir <- iris
ir$PL <- ir$Petal.Width > 1
ci_chisq_ncp(ir[, c("Species", "PL")])
ci_chisq_ncp(ir[, c("Species", "PL")], probs = c(0.05, 1))
}
\references{
\enumerate{
\item Smithson, M. (2003). Confidence intervals. Series: Quantitative Applications in the Social Sciences. New York, NY: Sage Publications.
\item Efron, B. and Tibshirani R. J. (1994). An Introduction to the Bootstrap. Chapman & Hall/CRC.
\item Canty, A and Ripley B. (2019). boot: Bootstrap R (S-Plus) Functions.
}
}
\seealso{
\code{\link{ci_cramersv}}.
}
