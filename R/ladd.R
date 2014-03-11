#'  Add to Lattice Plots
#' 
#' Simplified lattice plotting by adding additional elements to existing plots.
#'
#' @param x  callable graphical element to be added to a panel or panels in a lattice plot 
#' @param data a list containing objects that can be referred to in \code{x}.  Panel functions also 
#' have access to the data already used in the panel by the underlying lattice plot.  See 
#' \code{link[latticeExtra]{layer}} for details.
#' @param ... additional arguments passed to \code{\link[latticeExtra]{layer}}.
#' @param plot a lattice plot to add to.  Defaults to previous lattice plot.
#' 
#' 
#' @details
#' \code{ladd} is a wrapper around \code{\link[latticeExtra]{layer}} that simplifies 
#' certain common plotting additions.
#' 
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' 
#' @export
#' @examples
#' p <- xyplot(rnorm(100) ~rnorm(100))
#' print(p)
#' ladd(panel.abline(a=0,b=1))
#' ladd(panel.abline(h=0,col='blue'))
#' ladd(grid.text('Hello'))
#' ladd(grid.text(x=.95,y=.05,'text here',just=c('right','bottom')))
#' q <- xyplot(rnorm(100) ~rnorm(100)|factor(rbinom(100,4,.5)))
#' q <- update(q, layout=c(3,2))
#' print(q)
#' ladd(panel.abline(a=0,b=1))
#' ladd(panel.abline(h=0,col='blue'))
#' ladd( grid.text("(2,1)",gp=gpar(cex=3,alpha=.5)), columns=2, rows=1)
#' ladd( grid.text("p5",gp=gpar(cex=3,alpha=.5)), packets=5)
#' print(q)
#' ladd( grid.text(paste(current.column(), current.row(),sep=','), gp=gpar(cex=3,alpha=.5)) )
#' histogram( ~eruptions, data=faithful)
#' ladd(panel.densityplot(faithful$eruptions))
#'
#' @keywords graphics 

ladd <- function (x, data=NULL, ..., plot=trellis.last.object()) 
{
  return( plot + eval( substitute( latticeExtra::layer(foo, data=data, ...),
                                   list(foo=substitute(x) ) )
  ) )
}
