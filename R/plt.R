

#==========
# Create an environment for storing axis limits, etc.
.plotFun.envir = new.env(parent=baseenv())
# ==========
plotFun <- function(expr, ..., add=FALSE,
                   xlim=NULL,ylim=NULL,npts=NULL,
                   ylab=NULL, xlab=NULL, zlab=NULL, main=NULL, 
                   lwd=1,col="black",filled=TRUE,nlevels=10,
                   surface=FALSE,
                   colorscheme=topo.colors,type="l",transparency=NULL ) { 
  dots <- list(...)
  
  ..f.. <- .createMathFun( sexpr=substitute(expr), ...)

  vars <- formals(..f..$fun)

  ndims <- length(..f..$names)
  if( ndims == 1 ){
    npts <- ifelse( is.null(npts), 200, npts)
    # create a function of that one variable
    pfun <- function(.x){
	    mydots <- dots
      mydots[[..f..$names]] <- .x
      eval( ..f..$sexpr, envir=mydots, enclos=parent.frame())
    }

    # Set the axis labels
    # deparse needed for lattice (not originally for plot)
    if( is.null(ylab) ) ylab <- deparse(..f..$sexpr)
    if( is.null(xlab) ) xlab <- ..f..$names

    # figure out the limits.  
    # Is a limit specified, either through xlim or the variable name
    xlim2 <- xlim
    if( ..f..$names %in% names(dots) ) {
      xlim2 <- dots[[..f..$names]]
    }

    if( length(xlim2) < 2 ) { # no limits were specified
       if (add) xlim2 = get("xlim",.plotFun.envir)
       else {
        warning("No dependent variable limit set.") 
        xlim2 <- c(0,1)   # meaningless default
       }
    } 

    if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
      stop(paste("Must provide x-axis limit via ", 
                 ..f..$names, "= or xlim=", sep=""))
    }
    # Evaluate the function.
		.xset <- mosaic::adapt_seq(min(xlim2), max(xlim2), 
									 f=function(xxqq){ pfun(xxqq) }, length=npts)

  	.yset <- pfun(.xset)
	  if( length(.yset) != length(.xset) ){ # if pfun isn't vectorized, loop over .xset
		  .yset == rep(0, length(.xset)) 
		  for (k in 1:length(.xset) ) {
			 .yset[k] <- pfun(.xset[k]) 
			  dots[[..f..$names]] <- .xset[k]
	   	}
  	}

    if (add) { # add to existing plot using ladd()
      graphics::lines(.xset, .yset, lwd=lwd, col=col)
      # ladd(panel.lines(.xset, .yset, lwd=lwd, col=col, ...))       
	  } else { # draw a new plot
	    assign("xlim", xlim2, .plotFun.envir)
	    graphics::plot( .xset, .yset, type=type, 
	                    lwd=lwd, col=col, xlim=xlim2, ylim=ylim,
	                    xlab=xlab,ylab=ylab,main=main)
	    tmp = par("usr")[c(3,4)] # get the y limits of the plot
	    assign("ylim", tmp, .plotFun.envir)
	   
#       if( is.null(ylim)) thePlot <- lattice::xyplot(.yset ~ .xset, type=type,
#                       lwd=lwd, col=col, 
# 					            xlim=xlim2, 
#                       xlab=xlab,ylab=ylab,
# 					            main=main)
#       else thePlot <- lattice::xyplot(.yset ~ .xset, type=type,
#                                       lwd=lwd, col=col, 
#                                       xlim=xlim2, ylim=ylim, 
#                                       xlab=xlab,ylab=ylab,
#                                       main=main)
#       # goo <- par("usr") # get the limits of the plot
#       # ..currentAxisLimitY <- goo[c(3,4)]
#       # mosaic.par.set(currentAxisLimitX = ..currentAxisLimitX )
#       # mosaic.par.set(currentAxisLimitY = ..currentAxisLimitY )
#       return(thePlot)
    }  
  }
  if (ndims == 2 ) {
    npts <- ifelse( is.null(npts), 40, npts)
    # create a function of those two variables
    pfun <- function(.x,.y){
      dots[[..f..$names[1]]] <- .x
      dots[[..f..$names[2]]] <- .y
      eval( ..f..$sexpr, envir=dots, enclos=parent.frame())
    }
    if( length(ylab) == 0 ) ylab <- ..f..$names[2]
    if( length(xlab) == 0 ) xlab <- ..f..$names[1]
    if( length(zlab) == 0 ) zlab <- deparse(..f..$sexpr)
    xlim2 <- xlim
    ylim2 <- ylim
    if( ..f..$names[1] %in% names(dots) ) {
      xlim2 <- dots[[..f..$names[1]]]
    }
    if( ..f..$names[2] %in% names(dots) ) {
      ylim2 <- dots[[..f..$names[2]]]
    }
    if (add  | length(xlim2)==0 | length(ylim2) == 0) {
      if (length(xlim2)==0) xlim2 <- get("xlim", .plotFun.envir)
      if (length(ylim2)==0) ylim2 <- get("ylim", .plotFun.envir)
      add <- TRUE
    }
    
    if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
      stop(paste("Must provide x-axis limit via ", 
                 ..f..$names, "= or xlim=", sep=""))
    }
    
    .xset <- seq(min(xlim2),max(xlim2),length=npts)
    .yset <- seq(min(ylim2),max(ylim2),length=npts)
    #   zvals <- matrix( rep(0,length(.xset)*length(.yset)),nrow=length(.xset))
    #   for (k in 1:length(.xset)) {
    #    for (j in 1:length(.yset)) {
    #     zvals[k,j] <- pfun( .xset[k], .yset[j] )
    #    }
    #   }
    
    if( !add ){ # store the axis information
      assign('xlim', range(xlim2), .plotFun.envir)
      assign('ylim', range(ylim2), .plotFun.envir)
    }
    
    zvals <- outer(.xset, .yset, function(x,y){pfun(x,y)} )
    if( surface ) {
      grid <- expand.grid( .xset, .yset )
      grid$height <- c(zvals)
      if( require(manipulate) ) {
        manipulate(print(wireframe(height ~ Var1 + Var2, xlab=xlab,ylab=ylab,zlab=zlab,data=grid,drape=filled,
                                   shade=TRUE,screen=c(x=-90,y=rot,z=0),col=rgb(1,1,1,0))), 
                   rot <- slider(-180,180,step=5,initial=45,label="Rotation"))
        
      } 
      else {
        print(wireframe(height ~ Var1 + Var2, xlab=xlab,ylab=ylab,zlab=zlab,data=grid,drape=filled,shade=TRUE,
                        col=rgb(1,1,1,0)) )
      }
      ..currentAxisNames <- ..f..$names
      
      # No ADD method yet for surface plots
    }
    else {
      if( add & is.null(transparency) ) transparency<-.4
      if( is.null(transparency) ) transparency<-1
      fillcolors <- colorscheme(20,alpha=transparency)
      if( is.logical(zvals[1,1]) ){ # it's a constraint function
        if( add ) fillcolors<- c(rgb(0,0,0,transparency),rgb(0,0,0,0))
        else fillcolors <- colorscheme(2)
        nlevels<-2
      }
      if( filled) {
        graphics::image( .xset, .yset, zvals, col=fillcolors,add=add,
                         xlab=xlab,ylab=ylab,main=main )
        graphics::contour(.xset, .yset, zvals, col=col,lwd=lwd,add=TRUE, nlevels=nlevels)
      }
      else {
        graphics::contour(.xset,.yset,zvals,nlevels=nlevels,add=add,lwd=lwd,col=col,
                          xlab=xlab,ylab=ylab,main=main)
      } 
    }
    ..currentAxisNames <- ..f..$names
  }
  else if( ndims > 2 ) 
    stop("More than 2 plotting variables.")
  
  invisible(..f..$fun)
}
# =============================
