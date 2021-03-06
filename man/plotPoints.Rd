% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPoints.R
\name{plotPoints}
\alias{plotPoints}
\title{Scatter plot of points}
\usage{
plotPoints(
  x,
  data = parent.frame(),
  add = NULL,
  under = FALSE,
  panelfun = panel.xyplot,
  plotfun = xyplot,
  ...,
  plot = trellis.last.object()
)
}
\arguments{
\item{x}{A formula specifying y ~ x or z ~ x&y}

\item{data}{Data frame containing the variables to be plotted.  If not specified,
the variables will be looked up in the local environment}

\item{add}{If \code{TRUE}, add points as a new layer to an existing plot.
If \code{NULL}, the value of \code{under} will be used.}

\item{under}{If \code{TRUE}, the new layer will be underneath existing layers.}

\item{panelfun}{Lattice panel function to be used for adding.  Set only if you want something other
than a scatter plot.  Mainly, this is intended to add new functionality through other functions.}

\item{plotfun}{Lattice function to be used for initial plot creation. Set only
if you want something other than a scatter plot. Mainly, this is intended to add new functionality through
other functions.}

\item{\dots}{additional arguments}

\item{plot}{a trellis plot, by default the most recently created one.  If \code{add} is \code{TRUE},
new points will be added as a new layer to \code{plot}.}
}
\value{
A trellis graphics object
}
\description{
Make or add a scatter plot in a manner coordinated with \code{plotFun}.
}
\examples{
if (require(mosaicData)) {
plotPoints( width ~ length, data=KidsFeet, groups=sex, pch=20)
f <- makeFun( lm( width ~ length * sex, data=KidsFeet))
plotFun( f(length=length,sex="G")~length, add=TRUE, col="pink")
plotFun( f(length=length,sex="B")~length, add=TRUE)
}
}
\seealso{
\code{\link[=plotFun]{plotFun()}}
}
