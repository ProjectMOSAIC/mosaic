\docType{methods}
\name{project}
\alias{project}
\alias{project,formula-method}
\alias{project,formula,ANY-method}
\alias{project,matrix-method}
\alias{project,matrix,ANY-method}
\alias{project,numeric-method}
\alias{project,numeric,ANY-method}
\title{Projections}
\arguments{
  \item{x}{a vector or formula.  Left-hand sides of
  formulas should be a single quantity}

  \item{u}{a vector}

  \item{data}{a data frame.}

  \item{type}{one of \code{length} or \code{vector}
  determining the type of the returned value}

  \item{\dots}{additional arguments}
}
\value{
  \code{project} returns the projection of \code{x} onto
  \code{u} (or its length if \code{u} and \code{v} are
  numeric vectors and \code{type == "length"})
}
\description{
  Compute projections onto the span of a vector or a model
  space.
}
\details{
  \code{project} (preferably pronounced "pro-JECT" as in
  "projection") does either of two related things: (1)
  Given two vectors as arguments, it will project the first
  onto the second, returning the point in the subspace of
  the second that is as close as possible to the first
  vector.  (2) Given a formula as an argument, will work
  very much like \code{lm()}, constructing a model matrix
  from the right-hand side of the formula and projecting
  the vector on the left-hand side onto the subspace of
  that model matrix.  In (2), rather than returning the
  projected vector, \code{project()} returns the
  coefficients on each of the vectors in the model matrix.
  UNLIKE \code{lm()}, the intercept vector is NOT included
  by default.  If you want an intercept vector, include
  \code{+1} in your formula.
}
\examples{
a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
# projection onto the 1 vector gives the mean vector
mean(x)
project(x, 1)
# return the length of the vector, rather than the vector itself
project(x, 1, type='length')
project(a~b)
project(width~length+sex, data=KidsFeet)
project(log(width) ~ I(length^2)+sin(length)+sex, data=KidsFeet)
}

