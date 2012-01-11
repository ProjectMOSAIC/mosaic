\name{statTally}
\alias{statTally}
\title{Tally test statistics}
\usage{
  statTally(sample, rdata, FUN, direction = NULL,
    stemplot = dim(rdata)[direction] < 201,
    q = c(0.5, 0.9, 0.95, 0.99), fun, ...)
}
\arguments{
  \item{sample}{sample data}

  \item{rdata}{a matrix of randomly generated data under
  null hypothesis.}

  \item{FUN}{a function that computes the test statistic
  from a data set.}

  \item{direction}{1 or 2 indicating whether samples in
  \code{rdata} are in rows (1) or columns (2).}

  \item{stemplot}{indicates whether a stem plot should be
  displayed}

  \item{q}{quantiles of sampling distribution to display}

  \item{fun}{same as \code{FUN} so you don't have to
  remember if it should be capitalized}

  \item{\dots}{additional arguments passed to
  \code{\link{xhistogram}}}
}
\value{
  A lattice plot is returned invisibly but can be
  \code{print}ed.

  As side effects, some information about the empirical
  sampling distribution and optionally a stem plot are
  printed to the screen.
}
\description{
  Tally test statistics from data and from multiple draws
  from a simluated null distribution
}
\examples{
# is my spinner fair?
x <- c(10,18,9,15)   # counts in four cells
rdata <- rmultinom(1000, 4, prob=rep(.25,4))
print(statTally( x, rdata, fun=max ))  # unusual test statistic
print(statTally( x, rdata, fun=var ))  # equivalent to chi-squared test
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\keyword{inference}
\keyword{teaching}

