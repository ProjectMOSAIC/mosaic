#'  Add to Lattice Plots
#' 
#' Simplified lattice plotting by adding additional elements to existing plots.
#'
#' @param x  callable graphical element to be added to a panel or panels in a lattice plot 
#' @param col,row identifies desired panel(s) in multi-panel plots.  If missing, all
#'           columns or rows are used.
#' @param verbose a logical indicating whether to display some information about modified panels.
#' @param highlight a logical indicating whether to highlight panels as they are being modified.
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
#' ladd(panel.abline(h=0,col='blue'))
#' ladd(grid.text('Hello'))
#' ladd(grid.text(x=.95,y=.05,'text here',just=c('right','bottom')))
#' q <- xyplot(rnorm(100) ~rnorm(100)|factor(rbinom(100,4,.5)))
#' q <- update(q, layout=c(3,2))
#' print(q)
#' ladd(panel.abline(a=0,b=1))
#' ladd(panel.abline(h=0,col='blue'))
#' ladd( grid.text("(2,1)",gp=gpar(cex=3,alpha=.5)), 2, 1)
#' print(q)
#' ladd( grid.text(paste(current.column(), current.row(),sep=','), gp=gpar(cex=3,alpha=.5)) )
#'
#' @keywords graphics 

ladd <- function (x, col, row, highlight=FALSE, verbose=FALSE) 
{
	xUnevaluated <- substitute(x) 
	layout <- trellis.currentLayout('panel')
	if (missing(col)) { col <- 1:ncol(layout) }
	if (missing(row)) { row <- 1:nrow(layout) }

	for (r in row) {
		for (c in col) {
			if ( c >=1 && r >= 1 && c <= ncol(layout) && r <= nrow(layout) && layout[r,c] > 0 ) {
				if (verbose) 
					message(paste('  + Modifying panel ' ,layout[r,c], 
								  ' at position (',c,',',r,')...',sep=""))        
				trellis.focus("panel", c, r, highlight=highlight)
				eval.parent(xUnevaluated, n=1)
				trellis.unfocus()
			} else {
				if (verbose) 
					message(paste('  * No panel at position (',c,',',r,').',sep=""))
			}
		}
	}
}
