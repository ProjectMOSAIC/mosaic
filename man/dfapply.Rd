\name{dfapply}
\alias{dfapply}
\title{apply-type function for data frames}
\usage{
  dfapply(data, FUN, select = is.numeric, ...)
}
\arguments{
  \item{data}{data frame}

  \item{FUN}{a function to apply to (some) variables in the
  data frame}

  \item{select}{function used to select variables to which
  \code{FUN} is applied}

  \item{\dots}{arguments passed allong to \code{FUN}}
}
\description{
  An \code{apply()}-type function for data frames.
}
\examples{
dfapply(iris, favstats)
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\seealso{
  \code{\link{apply}}, \code{\link{sapply}},
  \code{\link{tapply}}, \code{\link{lapply}}
}

