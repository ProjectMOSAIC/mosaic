#'  Add to Lattice Plots
#' 
#' Simplified lattice plotting by adding additional elements to existing plots.
#'
#' @param x  callable graphical element to be added to a panel or panels in a lattice plot 
#' @param col,row identifies desired panel(s) in multi-panel plots.  If missing, all
#'           columns or rows are used.
#' 
#' 
#' @details
#' \code{ladd} is simply a wrapper around 
#' \code{\link{trellis.focus}} and \code{\link{trellis.unfocus}}. 
#' 
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' 
#' @export
#' @examples
#' p <- xyplot(rnorm(100) ~rnorm(100))
#' print(p)
#' ladd(panel.abline(a=0,b=1))
#' ladd(panel.abline(h=0,col='red'))
#' ladd(grid.text('Hello'))
#' ladd(grid.text(x=.95,y=.05,'text here',just=c('right','bottom')))
#' q <- update(p, layout=c(3,2))
#' ladd(panel.abline(a=0,b=1))
#' ladd(panel.abline(h=0,col='red'))
#' ladd( grid.text("(1,1)",gp=gpar(cex=3,alpha=.5)), 1,1)
#' ladd( grid.text("(2,1)",gp=gpar(cex=3,alpha=.5)), 2,1)
#' ladd( grid.text("(1,2)",gp=gpar(cex=3,alpha=.5)), 1,2)
#' ladd( grid.text("(2,2)",gp=gpar(cex=3,alpha=.5)), 2,2)
#'
#' @keywords graphics 
#' 
#' 
ladd <- function (x, col, row) 
{
  xUnevaluated <- substitute(x) 
  layout <- trellis.currentLayout('panel')
  if (missing(col)) { col <- 1:ncol(layout) }
  if (missing(row)) { row <- 1:nrow(layout) }
 
  for (r in row) {
    for (c in col) {
      cat(paste('Modifying panel (',c,',',r,')...\n',sep=""))
      trellis.focus("panel", c, r)
      eval.parent(xUnevaluated, n=2)
      trellis.unfocus()
    }
  }
}
