\name{plotDist}
\alias{plotDist}
\title{Plots of Discrete and Continuous Distributions}
\usage{
  plotDist(dist, params = list(),
    kind = c("density", "cdf", "qq", "histogram"),
    xlab = "", ylab = "", breaks = NULL, type,
    resolution = 5000, ...)
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

  \item{breaks}{a vector of break points for bins of
  histograms, as in \code{\link{histogram}}}

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
  \code{plotDist} determines whether the distribution is
  continuous or discrete by seeing if all the sampled
  quantiles are unique.  A discrete random variable with
  many possible values could fool this algorithm and be
  considered continuous.

  The plots are done referencing a data frame with
  variables \code{x} and \code{y} giving points on the
  graph of the pdf or pmf for the distribution.  This can
  be useful in conjuction with the \code{groups} argument.
  See the examples.
}
\examples{
plotDist('norm')
plotDist('norm', type='h')
plotDist('norm', kind='cdf')
plotDist('norm', params=list(mean=100, sd=10), kind='cdf')
plotDist('exp', kind='histogram')
plotDist('binom', params=list( 25, .25))
plotDist('binom', params=list( 25, .25), xlim=c(-1,26) )
plotDist('binom', params=list( 25, .25), kind='cdf')
plotDist('beta', params=list( 3, 10), kind='density')
plotDist('beta', params=list( 3, 10), kind='cdf')
plotDist( "binom", params=list(35,.25),
           groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25) )
plotDist( "binom", params=list(35,.25),
           groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25),
           kind='hist')
}
\keyword{graphics}
\keyword{stats}

