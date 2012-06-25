#'Plot the gradient field of a function
#'
#' @rdname vectorField
#' @name vectorField
#' @aliases vectorField
#'
#' @param object1 A formula specifying the behavior of the first variable.
#' @param object2 A formula specifying the behavior of the second variable.
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
#'#For a gradient field of the function x*y^2:
#'vectorField(D(x*y^2~x)(x,y)~x, D(x*y^2~y)(x,y)~y)
#'
#'#plot with long vectors
#'vectorField(x*y^2~x, x+y~y, vecLen=2)
#'
vectorField <- function(object1, object2, ..., add=FALSE, grad=FALSE,
                        xlim=NULL, ylim=NULL, npts=NULL,
                        ylab=NULL, xlab=NULL, groups=NULL,
                        col = FALSE, vecLen=1,
                        alpha=NULL){
  
  if (add) { 
    ladd( panel.vectorField( object1, object2,  npts=npts, alpha=alpha, ...))
    return(invisible(NULL))
  }
  
  dots <- list(...)
  dots[['alpha']] <- alpha
  
  fun1 = makeFun(object1, dots, strict.declaration=FALSE)
  fun2 = makeFun(object2, dots, strict.declaration=FALSE)
  
  vars <- union(formals(fun1), formals(fun2))###?
  rhsVars <- union(all.vars(rhs(object1)), all.vars(rhs(object2)))
  otherVars <- setdiff(names(vars), rhsVars)###?
  ndims <- length(rhsVars)
  if(length(all.vars(rhs(object1))) != 1||length(all.vars(rhs(object2))) != 1 )
    stop("Must have only one variable in the rhs of each formula.")
  cleanDots <- dots###?
  for (v in otherVars) {###?
    cleanDots[[v]] <- NULL###?
  }
  
  #ensure the formals of each function are the same.
  newVars = union(names(formals(fun1)), names(formals(fun2)))
  formals(fun1) <- 
    eval(parse( 
      text=paste( "as.pairlist(alist(", 
                  paste(newVars, "=", collapse=",", sep=""), "))"
      )
    ))
  formals(fun2) <- 
    eval(parse( 
      text=paste( "as.pairlist(alist(", 
                  paste(newVars, "=", collapse=",", sep=""), "))"
      )
    ))
  
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
  
  deltax = outer(.xvals, .yvals, fun1)
  deltay = outer(.xvals, .yvals, fun2)
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
  arrowlength = 1/20
  
  y=rep(0,0)
  x=rep(0,0)
  #creates a blank lattice plot and adds arrows
  myplot <- lattice::xyplot(
    y~x, xlab=xlab, ylab=ylab, 
    xlim=c(limits$xlim), ylim=c(limits$ylim),
    panel = function(x,y,...) {
      panel.arrows(grid$Var1, grid$Var2, grid$Var1+grid$dx, grid$Var2+grid$dy, 
                   length = arrowlength, lwd=1.5)}
  )
  return(myplot)
}

#' Panel function for plotting vector fields.
#'
#' @seealso vectorField
#' @param object1 an object (e.g., a formula) describing a function
#' @param object2 A formula specifying the behavior of the second variable.
#' @param npts an integer giving the number of points (in each dimension) to sample the function
#' 
#' @param alpha number from 0 (transparent) to 1 (opaque) for the fill colors 
#' @param vecLen scalar for length of output vectors.  Default is 1.
#' 
#' @param \dots additional arguments, typically processed by \code{lattice} panel functions
#'        such as \code{\link[lattice]{panel.xyplot}} or \code{\link[lattice]{panel.levelplot}}.
#'        Frequently used arguments include
#'        \describe{
#'          \item{\code{lwd}}{line width}
#'          \item{\code{lty}}{line type}
#'          \item{\code{col}}{a color}
#'        }
#'
panel.vectorField <- function(object1, object2, ..., npts=NULL,
                              ylab=NULL, xlab=NULL,
                              vecLen=1, alpha=NULL ){  
  dots <- list(...)
  dots[['alpha']] <- alpha #Not quite sure what alpha does yet.
  
  fun1 = makeFun(object1, dots, strict.declaration=FALSE)
  fun2 = makeFun(object2, dots, strict.declaration=FALSE)
  
  vars <- c(formals(fun1), formals(fun2))
  rhsVars <- union(all.vars(rhs(object1)), all.vars(rhs(object2)))
  otherVars <- setdiff(names(vars), rhsVars)
  ndims <- length(rhsVars)
  cleanDots <- dots
  for (v in otherVars) {
    cleanDots[[v]] <- NULL
  }
  
  if(length(all.vars(rhs(object1))) != 1||length(all.vars(rhs(object2))) != 1 )
    stop("Must have only one variable in the rhs of each formula.")
  
  #ensure the formals of each function are the same.
  newVars = union(names(formals(fun1)), names(formals(fun2)))
  formals(fun1) <- 
    eval(parse( 
      text=paste( "as.pairlist(alist(", 
                  paste(newVars, "=", collapse=",", sep=""), "))"
      )
    ))
  formals(fun2) <- 
    eval(parse( 
      text=paste( "as.pairlist(alist(", 
                  paste(newVars, "=", collapse=",", sep=""), "))"
      )
    ))
  
  parent.xlim <- current.panel.limits()$xlim
  parent.ylim <- current.panel.limits()$ylim
  
  npts <- ifelse( is.null(npts), 10, npts)
  
  .xvals <- seq(min(parent.xlim),max(parent.xlim),length=npts)
  .yvals <- seq(min(parent.ylim),max(parent.ylim),length=npts)
  stepSize <- diff(parent.xlim)/npts
  
  deltax = outer(.xvals, .yvals, fun1)
  deltay = outer(.xvals, .yvals, fun2)
  grid <- expand.grid(.xvals,.yvals)
  grid$dx = c(deltax)
  grid$dy = c(deltay)
  
  #rescale derivatives - we want the maximum length of an arrow to be the distance to the nearest arrow
  maxd = max(abs(grid$dx), abs(grid$dy))
  grid$dx = vecLen*(grid$dx/maxd)*(diff(parent.xlim)/npts)
  grid$dy = vecLen*(grid$dy/maxd)*(diff(parent.ylim)/npts)
  
  #ensure size of arrowhead does not overpower arrow.
  arrowlength = 1/20
  
  suppressWarnings(panel.arrows(grid$Var1, grid$Var2,grid$Var1+grid$dx,grid$Var2+grid$dy, length = arrowlength,lwd=1.5))
}