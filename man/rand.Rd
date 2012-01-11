\name{rand}
\alias{rand}
\title{Random Regressors}
\usage{
  rand(df = 1, rdist = rnorm, args = list(), nr,
    seed = NULL)
}
\arguments{
  \item{df}{degrees of freedom, i.e., number of regressors}

  \item{rdist}{random distribution function for sampling}

  \item{args}{arguments for \code{rdist}}

  \item{nr}{number of rows in resulting matrix.  This can
  often be omitted in the context of functions like
  \code{lm} where it is inferred from the data frame, if
  one is provided.}

  \item{seed}{seed for random number generation}
}
\value{
  A matrix of random variates with \code{df} columns. In
  its intended use, the number of rows will be selected to
  match the size of the data frame supplied to \code{lm}
}
\description{
  A utility function for producing random regressors.
}
\examples{
rand(2,nr=4)
rand(2,rdist=rpois, args=list(lambda=3), nr=4)
summary(lm( waiting ~ eruptions + rand(1), faithful))
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu}) and Randall
  Pruim (\email{rpruim@calvin.edu})
}
\keyword{distribution}
\keyword{regression}

