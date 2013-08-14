\name{histogram}
\alias{histogram}
\alias{panel.xhistogram}
\alias{prepanel.xhistogram}
\alias{xhistogram}
\alias{xhistogramBreaks}
\title{Augmented histograms}
\usage{
  histogram(x, data,
    breaks = lattice.getOption("histogram.breaks"), ...)

  xhistogram(x, data = NULL, panel = panel.xhistogram,
    type = "density", center = NULL, width = NULL, ...)

  xhistogramBreaks(x, center = NULL, width = NULL, nint,
    ...)

  prepanel.xhistogram(x, breaks = xhistogramBreaks, ...)

  panel.xhistogram(x,
    dcol = trellis.par.get("plot.line")$col, dlwd = 2,
    gcol = trellis.par.get("add.line")$col, glwd = 2,
    fcol = trellis.par.get("superpose.polygon")$col,
    dmath = dnorm, verbose = FALSE, dn = 100, args = NULL,
    labels = FALSE, density = FALSE, fit = NULL,
    start = NULL, type = "density", v, h, groups = NULL,
    center = NULL, width = NULL, breaks,
    nint = round(1.5 * log2(length(x)) + 1),
    stripes = c("vertical", "horizontal", "none"),
    alpha = 1, ...)
}
\arguments{
  \item{x}{a formula or a numeric vector}

  \item{data}{a data frame in which to evaluate \code{x}}

  \item{panel}{a panel function}

  \item{type}{one of \code{'density'}, \code{'count'}, or
  \code{'percent'}}

  \item{nint}{approximate number of bins}

  \item{breaks}{break points for histogram bins, a function
  for computing such, or a method \code{\link{hist}} knows
  about given as a character string.  By default,
  \code{\link[mosaic]{xhistogramBreaks}} is used.}

  \item{\dots}{additional arguments passed to
  \code{\link[lattice]{histogram}} and (by default when the
  \pkg{mosaic} package has been loaded) on to
  \code{\link{panel.xhistogram}}}

  \item{dcol}{color of density curve}

  \item{gcol}{color of guidelines}

  \item{fcol}{fill color for histogram rectangles}

  \item{dmath}{density function for density curve overlay}

  \item{verbose}{be verbose?}

  \item{dn}{number of points to sample from density curve}

  \item{dlwd,glwd}{like \code{lwd} but affecting the
  density line and guide lines, respectively}

  \item{args}{a list of additional arguments for
  \code{dmath}}

  \item{labels}{should counts/densities/precents be
  displayed or each bin?}

  \item{density}{overlay density?}

  \item{fit}{a character string describing the distribution
  to fit.  Known distributions include
  \code{"exponential"}, \code{"normal"}, \code{"lognormal"
  }, \code{"poisson"}, \code{"beta"}, \code{"geometric"},
  \code{"t"}, \code{"weibull"}, \code{"cauchy"},
  \code{"gamma"}, \code{"chisq"}, and \code{"chi-squared"}}

  \item{start}{numeric value passed to
  \code{\link[MASS]{fitdistr}}}

  \item{center}{center of one of the bins}

  \item{width}{width of the bins}

  \item{groups}{as per \code{\link[lattice]{histogram}}}

  \item{stripes}{one of \code{"vertical"},
  \code{"horizontal"}, or \code{"none"}, indicating how
  bins should be striped when \code{groups} is not
  \code{NULL}}

  \item{h,v}{a vector of values for additional horizontal
  and vertical lines}

  \item{alpha}{transparency level}
}
\value{
  a trellis object

  \code{xhistogramBreaks} returns a vector of break points
}
\description{
  The \pkg{mosaic} \code{histogram} adds some additional
  functionality to \code{\link[lattice]{histogram}} making
  it simpler to obtain certain common histogram adornments.
}
\examples{
histogram(~age | substance, HELPrct, v=35, fit='normal')
histogram(~age, HELPrct, labels=TRUE, type='count')
histogram(~age, HELPrct, groups=cut(age, seq(10,80,by=10)))
histogram(~age, HELPrct, groups=sex, stripes='horizontal')
histogram(~racegrp, HELPrct, groups=substance,auto.key=TRUE)
xhistogramBreaks(1:10, center=5, width=1)
xhistogramBreaks(1:10, center=5, width=2)
xhistogramBreaks(0:10, center=15, width=3)
xhistogramBreaks(1:100, center=50, width=3)
xhistogramBreaks(0:10, center=5, nint=5)
}
\seealso{
  \code{\link[lattice]{histogram}}, \code{xhistogramBreaks}
}

