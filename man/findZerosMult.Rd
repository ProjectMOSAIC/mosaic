\name{findZerosMult}
\alias{findZerosMult}
\title{Find the zeros of a function of two or more variables}
\usage{
  findZerosMult(..., npts = 10, rad = 5, near = 0,
    sortBy = "byx")
}
\arguments{
  \item{...}{arguments for values NOTE: if the system has
  more than one equation and the rhs variables do not match
  up, there will be an error.}

  \item{npts}{number of desired zeros to return}

  \item{rad}{radius around near in which to look for zeros}

  \item{near}{center of search for zeros}

  \item{sortBy}{options for sorting zeros for plotting.
  Options are 'byx', 'byy' and 'radial'.  The default value
  is 'byx'.}
}
\value{
  A data frame of numerical values which should all result
  in a value of zero when input into original function
}
\description{
  Compute numerically zeros of a function of two or more
  variables. All free variables (all but the variable on
  the right side) named in the expression must be assigned
  a value via \code{\ldots}
}
\details{
  sorts points in the domain according to the sign of the
  function value at respective points. Use continuity and
  uniroot to find zeros between points of opposite signs.
  Returns any number of points which may be sorted and
  plotted according to x, y, or radial values.
}
\examples{
findZerosMult(a*x^2-8~a&x, npts = 50)
findZerosMult(a^2+x^2-8~a&x, npts = 100, sortBy='radial')
\dontruni{findZerosMult(a^2+x^2-8~a&x, npts = 1000, sortBy='radial')}
}
\author{
  Cecylia Bocovich
}

