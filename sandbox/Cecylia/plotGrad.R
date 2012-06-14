#'
#'Plot the gradient field of a function
#'
#'
plotGrad <- function(object, ..., add=FALSE,
                    xlim=NULL, ylim=NULL, npts=NULL,
                    ylab=NULL, xlab=NULL, groups=NULL,
                    alpha=NULL ){
  
  if (add) { 
    ladd( panel.plotFun( object, npts=npts, alpha=alpha, ...))
    return(invisible(NULL))
  }
  
  dots <- list(...)
  dots[['alpha']] <- alpha
  
  ..f.. <- do.call( "makeFun", c(object, dots, strict.declaration=FALSE), envir= parent.frame())  
  
  vars <- formals(..f..)
  rhsVars <- all.vars(rhs(object))
  otherVars <- setdiff(names(vars), rhsVars)
  ndims <- length(rhsVars)
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
  grid$dx = (grid$dx/maxd)*(diff(limits$xlim)/npts)
  grid$dy = (grid$dy/maxd)*(diff(limits$ylim)/npts)
  
  #Widen limits to fit in arrows
  limits$xlim[1] = limits$xlim[1]-min(grid$dx)
  limits$xlim[2] = limits$xlim[2]+max(grid$dx)
  limits$ylim[1] = limits$ylim[1]-min(grid$dy)
  limits$ylim[2] = limits$ylim[2]+max(grid$dy)
  
  #ensure size of arrowhead does not overpower arrow.
  arrowlength = max(diff(limits$xlim)/(50), mean(grid$dx,grid$dy)/2)
  
  #myplot = lattice::xyplot(grid$Var1, grid$Var2, xlab=xlab, ylab=ylab,xlim=c(limits$xlim),ylim=c(limits$ylim))
  myplot=plot(0, cex=0, xlab=xlab, ylab=ylab,xlim=c(limits$xlim),ylim=c(limits$ylim))#need to change this to something else
  suppressWarnings(arrows(grid$Var1, grid$Var2,grid$Var1+grid$dx,grid$Var2+grid$dy, length = arrowlength,lwd=1.5))
  return(myplot)
}

#Still need to add
panel.plotGrad <- function(object, ..., add=FALSE,
                           xlim=NULL, ylim=NULL, npts=NULL,
                           ylab=NULL, xlab=NULL, groups=NULL,
                           alpha=NULL ){
  
  dots <- list(...)
  
  parent.xlim <- current.panel.limits()$xlim
  parent.ylim <- current.panel.limits()$ylim
}