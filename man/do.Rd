\name{do}
\alias{.clean_names}
\alias{.make.data.frame}
\alias{.merge_data_frames}
\alias{.squash_names}
\alias{*}
\alias{*,repeater,ANY-method}
\alias{do}
\alias{print}
\alias{print,repeater-method}
\title{Do Things Repeatedly}
\usage{
  do(n = 1L, cull = NULL, mode = NULL)

  .make.data.frame(x)

  .clean_names(x)

  .merge_data_frames(a, b)

  .squash_names(object, sep = ":")
}
\arguments{
  \item{n}{number of times to repeat}

  \item{cull}{function for culling output of objects being
  repeated.  If NULL, a default culling function is used.
  The default culling function is currently aware of
  objects of types \code{lme}, \code{lm}, \code{htest},
  \code{table}, \code{cointoss}, and \code{matrix}.}

  \item{mode}{target mode for value returned}

  \item{x}{object to be converted}

  \item{a}{a data frame}

  \item{b}{a data frame}

  \item{object}{an object}

  \item{sep}{a character}
}
\value{
  \code{do} returns an object of class \code{repeater}
  which is only useful in the context of the operator
  \code{*}.  See the examples.

  a data frame
}
\description{
  \code{do()} provides a natural syntax for repetition
  tuned to assist with replication and resampling methods.
}
\examples{
do(3) * rnorm(1)
do(3) * "hello"
do(3) * lm(shuffle(height) ~ sex + mother, Galton)
do(3) * summary(lm(shuffle(height) ~ sex + mother, Galton))
do(3) * 1:4
do(3) * mean(rnorm(25))
do(3) * c(mean = mean(rnorm(25)))
}
\author{
  Daniel Kaplan (\email{kaplan@macalaster.edu}) and Randall
  Pruim (\email{rpruim@calvin.edu})
}
\seealso{
  \code{\link{replicate}}
}
\keyword{internal}
\keyword{iteration}

