\name{ediff}
\alias{ediff}
\title{Lagged Differences with equal length}
\usage{
  ediff(x, lag = 1, differences = 1, frontPad = TRUE, ...)
}
\arguments{
  \item{x}{a numeric vector or a matrix containing the
  values to be differenced}

  \item{lag}{an integer indicating which lag to use}

  \item{differences}{an integer indicating the order of the
  difference}

  \item{frontPad}{logical indicating whether padding is on
  the front (default) or back end.}

  \item{\dots}{further arguments to be passed to or from
  methods}
}
\description{
  Often when creating lagged differences, it is awkward
  that the differences vector is shorter than the original.
  \code{ediff} pads with \code{NA}s to make its output the
  same length as the input.
}
\examples{
ediff(1:10)
ediff(1:10, 2)
ediff(1:10, 2, 2)
x <- cumsum(cumsum(1:10))
ediff(x, lag = 2)
ediff(x, differences = 2)
ediff(.leap.seconds)
Men <- subset(SwimRecords, sex=="M")
Men <- transform(Men, change=ediff(time), interval=ediff(year))
head(Men)
}
\seealso{
  \code{\link{diff}} since \code{ediff} is a thin wrapper
  around \code{\link{diff}}.
}

