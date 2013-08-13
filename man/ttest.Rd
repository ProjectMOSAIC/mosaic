\name{t.test}
\alias{t.test}
\alias{ttest}
\alias{ttest.default}
\alias{ttest.formula}
\title{Student's t-Test}
\usage{
  t.test(x, ...)

  ttest(x, ...)

  \method{ttest}{default} (x, ...)

  \method{ttest}{formula} (x, data = parent.frame(), ...)
}
\arguments{
  \item{x}{an object (e.g., a formula or a numeric vector)}

  \item{data}{a data frame}

  \item{\dots}{additional arguments, see
  \code{\link[stats]{t.test}} in the \pkg{stats} package.}
}
\value{
  an object of class \code{htest}
}
\description{
  Performs one and two sample t-tests. The mosaic
  \code{t.test} provides wrapper functions around the
  function of the same name in \pkg{stats}. These wrappers
  provide an extended interface that allows for a more
  systematic use of the formula interface.

  rdname ttest
}
\details{
  This is a wrapper around \code{\link{t.test}} from the
  \pkg{stats} package to extend the functionality of the
  formula interface.
}
\examples{
t.test( ~ age, data=HELPrct)
t.test( age ~ sex, data=HELPrct)
t.test( ~ age | sex, data=HELPrct)
t.test( ~ mother + father, data=Galton)
}
\seealso{
  \code{\link[mosaic]{prop.test}},
  \code{\link[stats]{t.test}}
}

