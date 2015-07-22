#' Scatter plot of points
#' 
#' Make or add a scatter plot in a manner coordinated with \code{plotFun}.
#' 
#' @param x A formula specifying y ~ x or z ~ x&y
#'
#' @param data Data frame containing the variables to be plotted.  If not specified, 
#' the variables will be looked up in the local environment
#'
#' @param add If \code{TRUE}, add points as a new layer to an existing plot.
#' If \code{NULL}, the value of \code{under} will be used.
#' 
#' @param under If \code{TRUE}, the new layer will be underneat existing layers.
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
#' @param plot a trellis plot, by default the most recently created one.  If \code{add} is \code{TRUE},
#' new points will be added as a new layer to \code{plot}.
#' 
#' @return A trellis graphics object 
#' 
#' @seealso \code{\link{plotFun}}
#' @examples
#' if (require(mosaicData)) {
#' plotPoints( width ~ length, data=KidsFeet, groups=sex, pch=20)
#' f <- makeFun( lm( width ~ length * sex, data=KidsFeet))
#' plotFun( f(length=length,sex="G")~length, add=TRUE, col="pink")
#' plotFun( f(length=length,sex="B")~length, add=TRUE)
#' }
#' @export

plotPoints <- function( x, data=parent.frame(), add=NULL, under=FALSE,
                        panelfun=panel.xyplot, plotfun=xyplot, ..., plot=trellis.last.object()
                        ) {
  if (is.null(add)) add <- under
  if (!add) return(plotfun(x, data=data, ...))
  # else we're adding on
    xpts <- evalSubFormula(rhs(x),data=data)
    ypts <- evalSubFormula(lhs(x),data=data)
    dots <- list(...)
    plot + latticeExtra::layer( 
      do.call( panelfun, c(list(xpts[[1]],ypts[[1]]),dots)),
               data=as.list(environment()),
               under=under)
}
