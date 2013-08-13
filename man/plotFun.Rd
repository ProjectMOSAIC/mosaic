\name{plotFun}
\alias{plotFun}
\title{Plotting mathematical expressions}
\usage{
  plotFun(object, ..., add = FALSE, xlim = NULL,
    ylim = NULL, npts = NULL, ylab = NULL, xlab = NULL,
    zlab = NULL, filled = TRUE, levels = NULL,
    nlevels = 10, labels = TRUE, surface = FALSE,
    groups = NULL,
    col = trellis.par.get("superpose.line")$col,
    col.regions = topo.colors, type = "l", alpha = NULL)
}
\arguments{
  \item{object}{a mathematical expression or a function "of
  one variable" which will converted to something
  intuitively equivalent to \code{object(x) ~ x}. (See
  examples)}

  \item{add}{if TRUE, then overlay an existing plot}

  \item{xlim}{limits for x axis (or use variable names, see
  examples)}

  \item{ylim}{limits for y axis (or use variable names, see
  examples)}

  \item{npts}{number of points for plotting.}

  \item{xlab}{label for x axis}

  \item{ylab}{label for y axis}

  \item{zlab}{label for z axis (when in surface-plot mode)}

  \item{col}{vector of colors for line graphs and contours}

  \item{filled}{fill with color between the contours
  (\code{TRUE} by default)}

  \item{levels}{levels at which to draw contours}

  \item{nlevels}{number of contours to draw (if
  \code{levels} not specified)}

  \item{labels}{if \code{FALSE}, don't label contours}

  \item{surface}{draw a surface plot rather than a contour
  plot}

  \item{col.regions}{a vector of colors or a function
  (\code{topo.colors} by default) for generating such}

  \item{type}{type of plot (\code{"l"} by default)}

  \item{alpha}{number from 0 (transparent) to 1 (opaque)
  for the fill colors}

  \item{groups}{grouping argument ala lattice graphics}

  \item{\dots}{additional parameters, typically processed
  by \code{lattice} functions such as
  \code{\link[lattice]{xyplot}},
  \code{\link[lattice]{levelplot}} or their panel
  functions. Frequently used parameters include \describe{
  \item{\code{main}}{main title for plot }
  \item{\code{sub}}{subtitle for plot }
  \item{\code{lwd}}{line width } \item{\code{lty}}{line
  type } \item{\code{col}}{a color } } Additionally, these
  arguments can be used to specify parameters for the
  function being plotted and to specify the plotting window
  with natural names.  See the examples for such usage.}
}
\value{
  a \code{trellis} object
}
\description{
  Plots mathematical expressions in one and two variables.
}
\details{
  makes plots of mathematical expressions using the formula
  syntax.  Will draw both line plots and contour/surface
  plots (for functions of two variables). In RStudio, the
  surface plot comes with sliders to set orientation. If
  the colors in filled surface plots are too blocky,
  increase \code{npts} beyond the default of 50, though
  \code{npts=300} is as much as you're likely to ever need.
  See examples for overplotting a constraint function on an
  objective function.
}
\examples{
plotFun( a*sin(x^2)~x, xlim=range(-5,5), a=2 )  # setting parameter value
plotFun( u^2 ~ u, ulim=c(-4,4) )                # limits in terms of u
# Note roles of ylim and y.lim in this example
plotFun( y^2 ~ y, ylim=c(-2,20), y.lim=c(-4,4) )
# Combining plot elements to show the solution to an inequality
plotFun( x^2 -3 ~ x, xlim=c(-4,4), grid=TRUE )
ladd( panel.abline(h=0,v=0,col='gray50') )
plotFun( (x^2 -3) * (x^2 > 3) ~ x, type='h', alpha=.1, lwd=4, col='lightblue', add=TRUE )
plotFun( sin(x) ~ x,
   groups=cut(x, findZeros(sin(x) ~ x, within=10)$x),
   col=c('blue','green'), lty=2, lwd=3, xlim=c(-10,10) )
## plotFun( sin(2*pi*x/P)*exp(-k*t)~x+t, k=2, P=.3)
f <- rfun( ~ u & v )
plotFun( f(u=u,v=v) ~ u & v, u.lim=range(-3,3), v.lim=range(-3,3) )
plotFun( u^2 + v < 3 ~ u & v, add=TRUE, npts=200 )
# display a linear model using a formula interface
model <- lm(wage ~ poly(exper,degree=2), data=CPS85)
fit <- makeFun(model)
xyplot(wage ~ exper, data=CPS85)
plotFun(fit(exper) ~ exper, add=TRUE, lwd=8)
# Can also just give fit since it is a "function of one variable"
plotFun(fit, add=TRUE, lwd=2, col='white')
# Attempts to find sensible axis limits by default
plotFun( sin(k*x)~x, k=0.01 )
}

