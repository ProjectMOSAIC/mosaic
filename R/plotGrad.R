#'Plot the gradient field of a function
#'
#' @rdname plotGrad
#' @name plotGrad
#' @aliases plotGrad
#'
#' @param object a mathematical expression (see examples)
#' @param add if TRUE, then overlay an existing plot
#' @param xlim limits for x axis (or use variable names, see examples)
#' @param ylim limits for y axis (or use variable names, see examples)
#' @param npts number of points for plotting. 
#' @param xlab label for x axis
#' @param ylab label for y axis
#' 
#' @param col color for line graphs and contours
#' @param vecLen scalar for length of output vectors.  Default is 1.
#' 
#' @param alpha number from 0 (transparent) to 1 (opaque) for the fill colors 
#' @param groups grouping argument ala lattice graphics
#' @param \dots additional parameters, typically processed by \code{lattice} functions such as 
#' \code{\link[lattice]{xyplot}}, \code{\link[lattice]{levelplot}} or their panel functions.  
#' Frequently used parameters include 
#' \describe{ 
#' \item{\code{main}}{main title for plot }
#' \item{\code{sub}}{subtitle for plot }
#' \item{\code{lwd}}{line width }
#' \item{\code{lty}}{line type }
#' \item{\code{col}}{a color } 
#' }
#' 
#' Additionally, these arguments can be used to specify parameters for the function being 
#' plotted and to specify the plotting window with natural names.  See the examples for such usage.
#'
#' @return a \code{trellis} object
#'
#' @details
#' makes gradient plots of mathematical expressions using the formula syntax.  To change the size of
#' arrow vectors, change the default arrowLength value.  To plot more arrows, change npts.
#' 
#' Calculates partial derivatives using in each direction of variables.
#' 
#'@examples
#'
#'plotGrad(x*y^2~x&y)
#'plotGrad(x*y^2~x&y, xlim=c(-2,2), ylim = c(-2,2))
#'plotGrad(x*y^2~x&y, xlim=c(-2,2), ylim = c(-2,2), npts = 20)
#'
#'#plot with long vectors
#'plotGrad(x*y^2~x&y, vecLen=2)
#'
plotGrad <- function(object, ..., add=FALSE,
                    xlim=NULL, ylim=NULL, npts=NULL,
                    ylab=NULL, xlab=NULL, groups=NULL,
                    col = FALSE, vecLen=1,
                    alpha=NULL){
  
  if (add) { 
    ladd( panel.plotGrad( object, npts=npts, alpha=alpha, ...))
    return(invisible(NULL))
  }
  
  dots <- list(...)
  dots[['alpha']] <- alpha
  
  ..f.. <- do.call( "makeFun", c(object, dots, strict.declaration=FALSE), envir= parent.frame())  
  
  vars <- formals(..f..)
  rhsVars <- all.vars(rhs(object))
  otherVars <- setdiff(names(vars), rhsVars)
  ndims <- length(rhsVars)
  if(ndims<2|| ndims>2) stop("Incorrect number of dimensions.")
  cleanDots <- dots
  for (v in otherVars) {
    cleanDots[[v]] <- NULL
  }
  
  limits <- inferArgs( dots=dots, vars=rhsVars, defaults=list(xlim=xlim, ylim=ylim) )

  npts <- ifelse( is.null(npts), 10, npts)
  if( length(ylab) == 0 ) ylab <- rhsVars[2]
  if( length(xlab) == 0 ) xlab <- rhsVars[1]
  
  if (is.null(limits$xlim) || length(limits$xlim) < 2 ) 
    limits$xlim <- c(0,1)  # temporary default
  else 
    limits$xlim <- range(limits$xlim)
  
  if (is.null(limits$ylim) || length(limits$ylim) < 2 ) 
    limits$ylim <- c(0,1)  # temporary default
  else 
    limits$ylim <- range(limits$ylim)
  
  
  .xvals <- seq(min(limits$xlim),max(limits$xlim),length=npts)
  .yvals <- seq(min(limits$ylim),max(limits$ylim),length=npts)
  stepSize <- diff(limits$xlim)/npts
    
  obj = object
  obj[[3]]<- parse(text=rhsVars[1])[[1]] #replace rhs with on var
  partial1 = D(obj)#with respect to first var
  obj[[3]]<- parse(text=rhsVars[2])[[1]] #replace rhs with other var
  partial2 = D(obj)#with respect to second var
  deltax = outer(.xvals, .yvals, function(x,y){partial1(x,y)})
  deltay = outer(.xvals, .yvals, function(x,y){partial2(x,y)})
  grid <- expand.grid(.xvals,.yvals)
  grid$dx = c(deltax)
  grid$dy = c(deltay)
  
  #rescale derivatives - we want the maximum length of an arrow to be the distance to the nearest arrow
  maxd = max(abs(grid$dx), abs(grid$dy))
  grid$dx = vecLen*(grid$dx/maxd)*(diff(limits$xlim)/npts)
  grid$dy = vecLen*(grid$dy/maxd)*(diff(limits$ylim)/npts)
  
  #Widen limits to fit in arrows
  limits$xlim[1] = limits$xlim[1]-min(grid$dx)
  limits$xlim[2] = limits$xlim[2]+max(grid$dx)
  limits$ylim[1] = limits$ylim[1]-min(grid$dy)
  limits$ylim[2] = limits$ylim[2]+max(grid$dy)
  
  #ensure size of arrowhead does not overpower arrow.
  arrowlength = max(diff(limits$xlim)/(50), mean(grid$dx,grid$dy)/2)
  
  #Add optional color to gradient vectors specifying length
  if(col==TRUE){
    colors = topo.colors(4)
    colvec = sqrt(grid$dx^2+grid$dy^2)#find and normalize length
    colvec = colvec/max(colvec)*4   #of vectors
    colvec = ceiling(colvec)
  }
  else colvec=par("fg")#default
  
  y=rep(0,0)
  x=rep(0,0)
  #creates a black lattice plot and adds arrows
  myplot <- lattice::xyplot(
              y~x, xlab=xlab, ylab=ylab, 
              xlim=c(limits$xlim), ylim=c(limits$ylim),
              panel = function(x,y,...) {
                          panel.arrows(grid$Var1, grid$Var2, grid$Var1+grid$dx, grid$Var2+grid$dy, 
                          length = arrowlength, lwd=1.5)}
  )
#  suppressWarnings(panel.arrows(grid$Var1, grid$Var2,grid$Var1+grid$dx,grid$Var2+grid$dy, length = arrowlength,lwd=1.5))
  #myplot=plot(0, cex=0, xlab=xlab, ylab=ylab,xlim=c(limits$xlim),ylim=c(limits$ylim))#need to change this to something else
  #suppressWarnings(arrows(grid$Var1, grid$Var2,grid$Var1+grid$dx,grid$Var2+grid$dy, col=colvec, length = arrowlength,lwd=1.5))
  return(myplot)
}

#' Panel function for plotting gradient
#'
#' @seealso plotGrad
#' @param object an object (e.g., a formula) describing a function
#' @param npts an integer giving the number of points (in each dimension) to sample the function
#' 
#' @param xlab label for x axis
#' @param ylab label for y axis
#' 
#' @param alpha number from 0 (transparent) to 1 (opaque) for the fill colors 
#' @param vecLen scalar for length of output vectors.  Default is 1.
#' 
#' @param \dots additional arguments, typically processed by \code{lattice} panel functions
#'        such as \code{\link[lattice]{panel.xyplot}} or \code{\link[lattice]{panel.levelplot}}.
#'        Frequently used arguments include
#'        \describe{
#'          \item{\code{lwd}}{line width}
#'        	\item{\code{lty}}{line type}
#'        	\item{\code{col}}{a color}
#'        }
#'
panel.plotGrad <- function(object, ..., npts=NULL,
                           ylab=NULL, xlab=NULL,
                           vecLen=1, alpha=NULL ){  
  dots <- list(...)
  dots[['alpha']] <- alpha #Not quite sure what alpha does yet.
  
  ..f.. <- do.call( "makeFun", c(object, dots, strict.declaration=FALSE), envir= parent.frame())  
  
  vars <- formals(..f..)
  rhsVars <- all.vars(rhs(object))
  otherVars <- setdiff(names(vars), rhsVars)
  ndims <- length(rhsVars)
  cleanDots <- dots
  for (v in otherVars) {
    cleanDots[[v]] <- NULL
  }
  
  parent.xlim <- current.panel.limits()$xlim
  parent.ylim <- current.panel.limits()$ylim
  
  npts <- ifelse( is.null(npts), 10, npts)
  
  .xvals <- seq(min(parent.xlim),max(parent.xlim),length=npts)
  .yvals <- seq(min(parent.ylim),max(parent.ylim),length=npts)
  stepSize <- diff(parent.xlim)/npts
  
  obj = object
  obj[[3]]<- parse(text=rhsVars[1])[[1]] #replace rhs with on var
  partial1 = D(obj)#with respect to first var
  obj[[3]]<- parse(text=rhsVars[2])[[1]] #replace rhs with other var
  partial2 = D(obj)#with respect to second var
  
  deltax = outer(.xvals, .yvals, function(x,y){partial1(x,y)})
  deltay = outer(.xvals, .yvals, function(x,y){partial2(x,y)})
  grid <- expand.grid(.xvals,.yvals)
  grid$dx = c(deltax)
  grid$dy = c(deltay)
  
  #rescale derivatives - we want the maximum length of an arrow to be the distance to the nearest arrow
  maxd = max(abs(grid$dx), abs(grid$dy))
  grid$dx = vecLen*(grid$dx/maxd)*(diff(parent.xlim)/npts)
  grid$dy = vecLen*(grid$dy/maxd)*(diff(parent.ylim)/npts)

  #ensure size of arrowhead does not overpower arrow.
  arrowlength = max(diff(parent.xlim)/(50), mean(grid$dx,grid$dy)/2)

  suppressWarnings(panel.arrows(grid$Var1, grid$Var2,grid$Var1+grid$dx,grid$Var2+grid$dy, length = arrowlength,lwd=1.5))
  
}