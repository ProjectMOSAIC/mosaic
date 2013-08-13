\name{compareMean}
\alias{compareMean}
\title{Compare means between 2 groups}
\usage{
  compareMean(formula, data = NULL, ...)
}
\arguments{
  \item{formula}{a formula}

  \item{data}{a data frame in which \code{x} is evaluated
  if \code{x} is a formula.}

  \item{\dots}{other arguments}
}
\value{
  the difference in means between the second and first
  group
}
\description{
  A function to calculate the difference between the means
  two of groups for a continuous outcome variable
}
\examples{
data(HELPrct)
# calculate the observed difference
mean(age ~ sex, data=HELPrct)
obs <- compareMean(age ~ sex, data=HELPrct); obs
# calculate the permutation distribution
nulldist <- do(100) * compareMean(age ~ shuffle(sex),
  data=HELPrct)
histogram(~ result, groups=(result >= obs), nulldist,
  xlab="difference in means")
}
\seealso{
  \code{\link{do}}, \code{\link{compareProportion}} and
  \code{\link{shuffle}}
}
\keyword{iteration}
\keyword{stats}

