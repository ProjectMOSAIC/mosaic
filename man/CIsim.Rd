\name{CIsim}
\alias{CIsim}
\title{Compute confidence intervals from (multiple) simulated data sets}
\usage{
  CIsim(n, samples = 100, rdist = rnorm, args = list(),
    estimand = 0, conf.level = 0.95, method = t.test,
    method.args = list(), interval = function(x) {
        do.call(method, c(list(x, conf.level = conf.level), method.args))$conf.int
    }, estimate = function(x) {
        do.call(method, c(list(x, conf.level = conf.level), method.args))$estimate
    }, verbose = TRUE)
}
\arguments{
  \item{n}{size of each sample}

  \item{samples}{number of samples to simulate}

  \item{rdist}{function used to draw random samples}

  \item{args}{arguments required by \code{rdist}}

  \item{estimand}{true value of the parameter being
  estimated}

  \item{conf.level}{confidence level for intervals}

  \item{method}{function used to compute intervals.
  Standard functions that produce an object of class
  \code{htest} can be used here.}

  \item{method.args}{arguments required by \code{method}}

  \item{interval}{a function that computes a confidence
  interval from data.  Function should return a vector of
  length 2.}

  \item{estimate}{a function that computes an estimate from
  data}

  \item{verbose}{print summary to screen?}
}
\value{
  A data frame with variables \code{lower}, \code{upper},
  \code{estimate}, \code{cover} ('Yes' or 'No'), and
  \code{sample} is returned invisibly.  See the examples
  for a way to use this to display the intervals
  graphically.
}
\description{
  This function automates the calculation of coverage rates
  for exploring the robustness of confidence interval
  methods.
}
\examples{
CIsim(10,1000)    # 1000 95\% intervals using t.test; population is N(0,1)
CIsim(10,1000, rdist=rexp, estimand=1)    # this time population is Exp(1)
xYplot(Cbind(estimate,lower,upper) ~ sample,
		data=CIsim(10,100, rdist=rexp, estimand=1),
		par.settings=col.mosaic(),
		groups=cover)
ladd(panel.abline(h=1))
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\keyword{inference}
\keyword{simulation}

