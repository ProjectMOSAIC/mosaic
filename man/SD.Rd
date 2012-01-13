\name{SD}
\alias{SD}
\title{Compute standard deviation}
\usage{
  SD(x)
}
\arguments{
  \item{x}{a vector or formula}
}
\value{
  a numeric containing the standard deviaiton
}
\description{
  This computes the standard deviation as the square root
  of variance to avoid direct use of
  \code{\link[stats]{sd}}.
}
\note{
  The primary reason for this function is that
  \code{\link[stats]{sd}} generates warnings when used with
  \code{\link[Hmisc]{summary.formula}} from the
  \code{lattice} package.
}
\examples{
x <- rnorm(10)
SD(x)
sd(x)
summary(age ~ substance, data=HELPrct, fun=SD)
}
\seealso{
  \code{\link[mosaic]{sd}}
}

