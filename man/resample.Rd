\name{resample}
\alias{deal}
\alias{resample}
\alias{sample}
\alias{sample.data.frame}
\alias{sample.default}
\alias{sample.factor}
\alias{sample.matrix}
\alias{shuffle}
\title{More Random Samples}
\usage{
  resample(x, size, replace = TRUE, prob = NULL,
    groups = NULL, orig.ids = FALSE, ...)

  deal(x, size, replace = FALSE, prob = NULL,
    groups = NULL, orig.ids = FALSE)

  shuffle(x, replace = FALSE, prob = NULL, groups = NULL,
    orig.ids = FALSE)

  sample(x, size, replace = FALSE, ...)

  \method{sample}{default} (x, size, replace = FALSE,
    prob = NULL, groups = NULL, orig.ids = FALSE, ...)

  \method{sample}{data.frame} (x, size, replace = FALSE,
    prob = NULL, groups = NULL, orig.ids = TRUE,
    fixed = names(x), shuffled = c(),
    invisibly.return = NULL, ...)

  \method{sample}{matrix} (x, size, replace = FALSE,
    prob = NULL, groups = NULL, orig.ids = FALSE, ...)

  \method{sample}{factor} (x, size, replace = FALSE,
    prob = NULL, groups = NULL, orig.ids = FALSE,
    drop.unused.levels = FALSE, ...)
}
\arguments{
  \item{x}{Either a vector of one or more elements from
  which to choose, or a positive integer.}

  \item{size}{a non-negative integer giving the number of
  items to choose.}

  \item{replace}{Should sampling be with replacement?}

  \item{prob}{A vector of probability weights for obtaining
  the elements of the vector being sampled.}

  \item{groups}{groups to sample within (works much like
  \code{groups} in lattice plots)}

  \item{orig.ids}{a logical; should origianal ids be
  included in returned data frame?}

  \item{\dots}{additional arguments passed to
  \code{\link[base]{sample}}}

  \item{fixed}{a vector of column names}

  \item{shuffled}{a vector of column names}

  \item{invisibly.return}{a logical, should return be
  invisible?}

  \item{drop.unused.levels}{a logical, should unused levels
  be dropped?}
}
\description{
  These functions simplify and unify sampling in various
  ways.
}
\details{
  These functions are wrappers around \code{\link{sample}}
  providing different defaults and natural names.
}
\examples{
# 100 Bernoulli trials -- no need for replace=TRUE
resample(c(0,1), 100)
deal(Cards, 13)    # A Bridge hand
shuffle(Cards)
}

