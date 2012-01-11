\name{rfun}
\alias{rfun}
\title{Produce a random function that is a low-degree polynomial}
\usage{
  rfun(vars = ~x & y, seed = NULL, n = 0)
}
\arguments{
  \item{vars}{a formula; the LHS is empty and the RHS
  indicates the variables used for input to the function
  (separated by &)}

  \item{seed}{seed for random number generator, passed to
  \code{\link{set.seed}}.}

  \item{n}{****** needs documentation ******}
}
\value{
  a function with the appropriate number of inputs
}
\description{
  Produce a random function that is a low-degree polynomial
}
\examples{
rfun( ~ u & v)
}

