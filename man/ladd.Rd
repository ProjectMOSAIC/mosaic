\name{ladd}
\alias{ladd}
\title{Add to Lattice Plots}
\usage{
  ladd(x, col = 1, row = 1)
}
\arguments{
  \item{x}{callable graphical element to be added to a
  panel in a lattice plot}

  \item{row}{row identifying desired panel in multi-panel
  plots.}

  \item{col}{column identifying desired panel in
  multi-panel plots.}
}
\description{
  Simplified lattice plotting by adding additional elements
  to existing plots.
}
\details{
  \code{ladd} is simply a wrapper around
  \code{\link{trellis.focus}} and
  \code{\link{trellis.unfocus}}.
}
\examples{
ladd <- function(x,col=1,row=1) { trellis.focus('panel',col,row); x ; trellis.unfocus()
print(xyplot(rnorm(100) ~rnorm(100)))
ladd(panel.abline(a=0,b=1))
ladd(panel.abline(h=0,col='red'))
ladd(grid.text('Hello'))
ladd(grid.text(x=.95,y=.05,'text here',just=c('right','bottom')))
print(xyplot( rnorm(100) ~ rnorm(100) | factor(rbinom(100,3,.5)), layout=c(2,2) ))
ladd( grid.text("(1,1)",gp=gpar(cex=3,alpha=.5)), 1,1)
ladd( grid.text("(2,1)",gp=gpar(cex=3,alpha=.5)), 2,1)
ladd( grid.text("(1,2)",gp=gpar(cex=3,alpha=.5)), 1,2)
ladd( grid.text("(2,2)",gp=gpar(cex=3,alpha=.5)), 2,2)
}
}
\author{
  Randall Pruim (\email{rpruim@calvin.edu})
}
\keyword{graphics}

