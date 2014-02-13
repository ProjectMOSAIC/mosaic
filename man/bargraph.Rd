\name{bargraph}
\alias{bargraph}
\title{Create bar graphs from raw data}
\usage{
  bargraph(x, data = parent.frame(), groups,
    horizontal = FALSE, origin = 0,
    ylab = ifelse(horizontal, "", "Frequency"),
    xlab = ifelse(horizontal, "Frequency", ""), subset, ...)
}
\arguments{
  \item{x}{a formula describing the plot}

  \item{data}{a data frame in which the formula \code{x} is
  evaluated}

  \item{groups}{a variable or expression used for grouping.
  See \code{\link[lattice]{barchart}}.}

  \item{subset}{a vector describing a subset of \code{data}.  May be 
	  an expression which will be evaluated in \code{data}.}

  \item{horizontal}{a logical indicating whether bars
  should be horizontal}

  \item{\dots}{additional arguments passed to
  \code{\link[lattice]{barchart}}}

  \item{origin}{beginning point for bars.  For the default
  behavior used by \code{\link[lattice]{barchart}} set
  \code{origin} to \code{NULL}, but 0 is often a better
  default. If 0 is not good, perhaps you should use a
  different kind of plot as the results may be misleading.}

  \item{ylab}{a character vector of length one used for the
  y-axis label}

  \item{xlab}{a character vector of length one used for the
  x-axis label}
}
\value{
  a trellis object describing the plot
}
\description{
  \code{\link[lattice]{barchart}} from the \code{lattice}
  package makes bar graphs from pre-tabulated data.  Raw
  data can be tabulated using \code{\link{xtabs}}, but the
  syntax is unusual compared to the other lattice plotting
  functions.  \code{bargraph} provides an interface that is
  consistent with the other \code{lattice} functions.
}
\examples{
data(HELPrct)
bargraph( ~ substance, data=HELPrct)
bargraph( ~ substance, data=HELPrct, horizontal=TRUE)
bargraph( ~ substance | sex, groups=homeless, auto.key=TRUE, data=HELPrct)
}
\seealso{
  \code{\link[lattice]{barchart}}
}

