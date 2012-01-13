\name{qdata}
\alias{ddata}
\alias{pdata}
\alias{qdata}
\alias{rdata}
\title{Compute quantiles from data}
\usage{
  qdata(p, vals, data = NULL, ...)

  pdata(q, vals, data = NULL, lower.tail = TRUE, ...)

  rdata(n, vals, data = NULL, replace = TRUE, ...)

  ddata(x, vals, data = NULL, log = FALSE, ...)
}
\arguments{
  \item{p}{a vector of probabilities}

  \item{vals}{a vector containing the data}

  \item{data}{a data frame in which to evaluate vals}

  \item{\dots}{additional arguments passed to
  \code{quantile} or \code{sample}}

  \item{q}{a vector of quantiles}

  \item{lower.tail}{a logical indicating whether to use the
  lower or upper tail probability}

  \item{n}{number of values to sample}

  \item{replace}{a logical indicating whether to sample
  with replacement}

  \item{x}{a vector of quantiles}

  \item{log}{a logical indicating whether the result should
  be log transformed}
}
\value{
  For \code{qdata}, a vector of quantiles

  For \code{pdata}, a vector of probabilities

  For \code{rdata}, a vector of values sampled from
  \code{vals}

  For \code{ddata}, a vector of probabilities (empirical
  densities)
}
\description{
  \code{qdata} is a wrapper around \code{\link{quantile}}
  that makes the syntax more like the syntax for quantiles
  from theoretical distributions

  \code{pdata} computes cumulative probabilities from data.

  \code{rdata} randomly samples from data. It is a wrapper
  around \code{sample} that unifies syntax.

  \code{ddata} computes a probability mass function from
  data.
}
\examples{
data(iris)
qdata(.5, iris$Sepal.Length)
qdata(.5, Sepal.Length, data=iris)
data(iris)
pdata(3:6, iris$Sepal.Length)
pdata(3:6, Sepal.Length, data=iris)
data(iris)
rdata(10,iris$Species)
rdata(10, Species, data=iris)
data(iris)
ddata('setosa', iris$Species)
ddata('setosa', Species, data=iris)
}

