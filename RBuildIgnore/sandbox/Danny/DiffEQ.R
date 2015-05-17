#' Utilities for solving and plotting differential equations
#' 
#' These functions are used in the phase-plane plotting system, mPP.
#' They are currently written in base graphics, not lattice.
#' 
#' @rdname diffeq.Rd
#' @name DiffEqFuns
#' @alias showTraj showNullclines jacobianAtPoint trajPlot solnPlot flowPlot solveDE rkintegrate

### Written in Dec. 2010 by DTK ###

# put this into demo(phaseplane)

# =====================================
showTraj = function(tdur=1, col="red",add=TRUE,x=NULL,y=NULL) {
  fun <- current.dyn.system
  if( is.null(x) | is.null(y) ){
    cat("Click on the initial condition.\n")
    init <- as.numeric(locator(1))
  }
  else {
    init <- c(x=x,y=y)
  }
  names(init) <- names(formals(fun))
  soln <- solve.DE(fun, init=init, tlim=c(0,tdur), dataframe=TRUE )
  lines( soln$frame[[2]], soln$frame[[3]], col=col ) # time is the first one

  invisible(soln$funs)
}
# =====================================
showNullclines <- function(levels=c(0),resol=51,lwd=2) {
  fun <- current.dyn.system
  foo <- current.panel.limits()
  xlim <- foo$xlim; ylim <- foo$ylim
  x <- matrix(seq(xlim[1],xlim[2], length=resol), byrow=FALSE, resol,resol);
  y <- matrix(seq(ylim[1],ylim[2], length=resol),byrow=TRUE, resol, resol);
  npts <- resol*resol;
  z <- fun(x,y);
  z1 <- matrix(z[1:npts], resol, resol);
  z2 <- matrix(z[(npts+1):(2*npts)], resol, resol);
  panel.levelplot.raster(x,y,z1, subscripts = TRUE, at=c(-Inf, 0), col.regions=rgb(1,0,0,.1));
  panel.levelplot.raster(x,y,z2, subscripts = TRUE, at=c(-Inf, 0), col.regions=rgb(0,0,1,.1));
}

# =====================================
# Jacobian at a point.
jacobianAtPoint <- function(fun=NULL,x=NULL, y=NULL,h=0.000001){
if (is.null(fun) )  fun = current.dyn.system
if (is.null(x) | is.null(y)) {
  x0 <- locator(n=1);
  x <- x0$x; 
  y <- x0$y;  
}
  foo <- fun(x,y);
  foox <- fun(x+h,y);
  fooy <- fun(x,y+h);
  A <- (foox[1] - foo[1])/h;
  B <- (fooy[1] - foo[1])/h;
  C <- (foox[2] - foo[2])/h;
  D <- (fooy[2] - foo[2])/h;
  return(matrix( c(A,B,C,D ), 2,2, byrow=T))
}


# =====================================
trajPlot <- function(soln, n=1001, col="red") {
  t <- seq( soln$tlim[1], soln$tlim[2], length=n )
  one <- soln[[1]](t)
  two <- soln[[2]](t)
  flowPlot( soln$dynfun,
            xlim=range(one, na.rm=TRUE),
            ylim=range(two, na.rm=TRUE) )
  llines( one, two, col=col )
}
# =====================================
# plots out one or more solutions.
# The ... argument is the set of solutions to plot.
# this needs a better name -- what kind of solution?
solnPlot <- function(..., colfun=rainbow) {
  layout(rbind(1,2))
  solns <- list(...)
  
  if (length(solns) == 1 ) col <- c("black")
  else col <- c("black","blue",colfun(length(solns)))
  #par( mfrow=c(2,1))
  soln <- solns[[1]]
  curve( soln[[1]](x), min(soln[[3]]), max(soln[[3]]), xlab="t",
        ylab=names(soln)[1],n=1001,col=col[1])
  if( length(solns) > 1 ) {
    for (k in 2:length(solns) ) {
      soln <- solns[[k]]
      curve( soln[[1]](x), add=TRUE, n=1001,col=col[k],)
    }
  }
  soln <- solns[[1]]
  curve( soln[[2]](x), min(soln[[3]]), max(soln[[3]]), xlab="t",
        ylab=names(soln)[2],n=1001,col=col[1])
  if( length(solns) > 1 ) {
    for (k in 2:length(solns) ) {
      soln <- solns[[k]]
      curve( soln[[2]](x), add=TRUE,  n=1001,col=col[k])
    }
  }
  layout(1)
}
# =====================================
flowPlot <- function(fun,xlim=c(0,1), ylim=c(0,1), resol=10, col="black",
  add=FALSE,EW=NULL,NS=NULL,both=TRUE) {
  current.dyn.system <- fun
  arg.names <- names(formals(fun) )
  if (length( arg.names ) != 2 )
    stop("Must give dynamical function with two arguments.")
  if (add) {
    hoo <- par("usr")
    xlim <- hoo[1:2]
    ylim <- hoo[3:4]
  }
  else{
    panel.xyplot(1, xlim=xlim, ylim=ylim,
       xlab=arg.names[1], ylab=arg.names[2] )
  }
  x <- matrix(seq(xlim[1],xlim[2], length=resol), byrow=TRUE, resol,resol);
  y <- matrix(seq(ylim[1],ylim[2], length=resol),byrow=FALSE, resol, resol);
  npts <- resol*resol;
  xspace <- abs(diff(xlim))/(resol*5);
  yspace <- abs(diff(ylim))/(resol*5);
  x <- x + matrix(runif(npts, -xspace, xspace),resol,resol);
  y <- y + matrix(runif(npts, -yspace, yspace),resol,resol);
  z <- fun(x,y);
  z1 <- matrix(z[1:npts], resol, resol);
  z2 <- matrix(z[(npts+1):(2*npts)], resol, resol);
  maxx <- max(abs(z1));
  maxy <- max(abs(z2));
  dt <- min( abs(diff(xlim))/maxx, abs(diff(ylim))/maxy)/resol;
  lens <- sqrt(z1^2 + z2^2);
  lens2 <- lens/max(lens); 
  if( both ){
    larrows(c(x), c(y),
           c(x+dt*z1/((lens2)+.1)), c(y+dt*z2/((lens2)+.1)),
           length=.04, col=col);
  }
  if( !is.null(NS) ) {
    larrows(c(x), c(y),
           c(x), c(y+dt*z2/((lens2)+.1)),
           length=.04, col=NS);
  }
  if( !is.null(EW) ){
    larrows(c(x), c(y),
           c(x+dt*z1/((lens2)+.1)), c(y),
           length=.04, col=EW);
  }
    
    
}

# =====================================
# integrate a DE
solveDE <- function(fun, init=NULL, tlim=c(0,1), dataframe=FALSE ) {
  if( is.null( init ) )
    stop("Must provide initial condition.")

  # Set up the initial condition in the right order for the
  # dynamical function.
  dyn.args <- names(formals(fun))

  # create a vector-input function that calls the original
  if (1 == length(dyn.args) )
    fcall <- fun
  if (2 == length(dyn.args) )
    fcall <- function(xx){fun(xx[1], xx[2]) }
  if (3 == length(dyn.args) )
    fcall <- function(xx){fun(xx[1], xx[2], xx[3]) }
  if (4 == length(dyn.args) )
    fcall <- function(xx){fun(xx[1], xx[2], xx[3], xx[4]) }
  if (5 == length(dyn.args) )
    fcall <- function(xx){fun(xx[1], xx[2], xx[3], xx[4], xx[5]) }
  if (length(dyn.args) > 5 )
    stop("Too many variables in dynamical function.")

  
  dyn.init <- rep(0, length(dyn.args) )
  names( dyn.init ) <- dyn.args
  init.names <- names(init)

  for( k in 1:length(init) ) {
    if (!init.names[k] %in% dyn.args )
      stop( paste("Variable", init.names[k],
         "in initial condition is not one of the dynamical variables") )
    dyn.init[init.names[k] ] <- init[k]
  }

  # If t is one of the dynamical variables, set its initial value.
  if( "t" %in% dyn.args ) {
    if (dyn.init["t"] == 0) dyn.init["t"] <- min(tlim)
  }  
  foo <- rkintegrate( fcall, dyn.init, tstart=tlim[1],tend=tlim[2] )
  # return interpolating functions
  res <- list()
  for (k in 1:length(dyn.init) ) res[[k]] <- approxfun( foo$t, foo$x[,k])
  names(res) <- dyn.args
  res$tlim <- tlim
  res$dynfun <- fun

  if (dataframe) {
    # return a data frame
    res2 <- data.frame( t= foo$t )
    for (k in 1:length(dyn.init) ) {
      res2[dyn.args[k]] <- foo$x[,k]
    }
    return(list(funs=res,frame=res2))
  }
  #  return the interpolating functions
  return(res)
}
# ========================
# Runge-Kutta integration
rkintegrate <- function(fun,x0,tstart=0,tend=1) {
  dt <- if( tend > 0 ) min(.01, (tend - tstart)/100)
        else max(-.01, (tend-tstart)/100)
  nsteps <- round( .5+(tend-tstart)/dt);
  xout <- matrix(0,nsteps+1,length(x0));
  tout <- matrix(0,nsteps+1,1);
  tout[1] <- tstart;
  xout[1,] <- x0;
  for (k in 2:(nsteps+1)) {
      k1 <- dt*fun(x0);
      k2 <- dt*fun(x0+k1/2);
      k3 <- dt*fun(x0+k2/2);
      k4 <- dt*fun(x0+k3);
      x0 <- x0 + (k1+k4+(k2+k3)*2)/6;
      xout[k,] <- x0;
      tout[k] <- tout[k-1]+dt;
  }
  return( list(x=xout,t=tout) );
} 

