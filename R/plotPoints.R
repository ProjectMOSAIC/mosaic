#' Scatter plot of points
#' 
#' Make or add a scatter plot in a manner coordinated with \code{plotFun}.
#' 
#' @param x A formula specifying y ~ x or z ~ x&y
#'
#' @param data Data frame containing the variables to be plotted.  If not specified, 
#' the variables will be looked up in the local environment
#'
#' @param add If \code{TRUE}, overlay the scatter plot on the current plot.
#' 
#' @param panelfun Lattice panel function to be used for adding.  Set only if you want something other
#' than a scatter plot.  Mainly, this is intended to add new functionality through other functions.
#' 
#' @param plotfun Lattice function to be used for initial plot creation. Set only
#' if you want something other than a scatter plot. Mainly, this is intended to add new functionality through
#' other functions.
#' 
#' @param \dots additional arguments
#' 
#' @return A lattice graphics object (if \code{add=FALSE})
#' 
#' @seealso \code{\link{plotFun}}
#' @export
#' @examples
#' plotPoints( width ~ length, data=KidsFeet, groups=sex, pch=20)
#' f <- makeFun( lm( width ~ poly(length,2) * sex, data=KidsFeet))
#' plotFun( f(length=length,sex="G")~length, add=TRUE, col="pink")
#' plotFun( f(length=length,sex="B")~length, add=TRUE)

plotPoints <- function( x, data=parent.frame(),add=FALSE,
                        panelfun=panel.xyplot,plotfun=xyplot,...) {
  if (!add) return(plotfun(x, data=data, ...))
  else {
    xpts <- evalSubFormula(rhs(x),data=data)
    ypts <- evalSubFormula(lhs(x),data=data)
    ladd(panelfun(xpts[[1]],ypts[[1]],...))
  }
}