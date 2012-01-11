\name{distPlot}
\alias{distPlot}
\title{Plots of Discrete and Continuous Distributions}
\usage{
  distPlot(dist, params = list(),
    kind = c("density", "cdf", "qq", "histogram"),
    xlab = "", ylab = "", breaks, type, resolution = 5000,
    ...)
}
\arguments{
  \item{dist}{A string identifying the distribution.  This
  should work with any distribution that has associated
  functions beginning with 'd', 'p', and 'q' (e.g,
  \code{\link{dnorm}}, \code{\link{pnorm}}, and
  \code{\link{qnorm}}).  \code{dist} should match the name
  of the distribution with the initial 'd', 'p', or 'q'
  removed.}

  \item{params}{a list containing parameters for the
  distribution}

  \item{kind}{one of "density", "cdf", "qq", or "histogram"
  (or prefix of any of these)}

  \item{xlab,ylab}{as per other lattice functions}

  \item{breaks}{as per \code{\link{histogram}}}

  \item{type}{passed along to various lattice graphing
  functions}

  \item{resolution}{number of points to sample when
  generating the plots}

  \item{\dots}{other arguments passed along to lattice
  graphing routines}
}
\description{
  Provides a simple way to generate plots of pdfs,
  probability mass functions, cdfs, probability histograms,
  and normal-quantile plots for distributions known to R.
}
\details{
  \code{distPlot} determines whether the distribution is
  continuous or discrete by seeing if all the sampled
  quantiles are unique.  A discrete random variable with
  many possible values could fool this algorithm and be
  considered continuous.
}
\examples{
distPlot('norm')
distPlot('norm', type='h')
distPlot('norm', kind='cdf')
distPlot('norm', params=list(mean=100, sd=10), kind='cdf')
distPlot('exp', kind='histogram')
distPlot('binom', params=list( 25, .25))
distPlot('binom', params=list( 25, .25), xlim=c(-1,26) )
distPlot('binom', params=list( 25, .25), kind='cdf')
distPlot('beta', params=list( 3, 10), kind='density')
distPlot('beta', params=list( 3, 10), kind='cdf')
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\keyword{graphics}
\keyword{stats}

