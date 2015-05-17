#' Plotting mathematical expressions
#'
#' Plots mathematical expressions in one and two variables.  
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @rdname plotFun
#' @name plotFun
#' @aliases plotFun
#'
#' @param expr a mathematical expression (see examples)
#' @param ... additional assignments to parameters and limits
#' @param add if TRUE, then overlay an existing plot
#' @param xlim limits for x axis (or use variable names, see examples)
#' @param ylim limits for y axis (or use variable names, see examples)
#' @param npts number of points for plotting. 
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param zlab label for z axis (when in surface-plot mode)
#' @param main main label for plot
#' @param lwd line width for line graphs and contours
#' @param col color for line graphs and contours
#' @param filled fill with color between the contours (\code{TRUE} by default)
#' @param levels levels at which to draw contours
#' @param nlevels number of contours to draw (if \code{levels} not specified)
#' @param surface draw a surface plot rather than a contour plot
#' @param colorscheme function (\code{topo.colors} by default) for choosing colors for fill
#' @param type type of plot (\code{"l"} by default)
#' @param transparency number from 0 (transparent) to 1 (opaque) for the fill colors 
#'
#' @return an R function for the expression being plotted (not a graphics object)
#'
#' @details
#' makes plots of mathematical expressions using the formula syntax.  Will
#' draw both line plots and contour/surface plots (for functions of two variables)
#' In RStudio, the surface plot comes with sliders to set orientation.
#' If the colors in filled surface plots are two blocky, increase \code{npts} 
#' beyond the default of 50. \code{npts=300} is as much as you're likely to every need
#' See examples for overplotting a constraint function on an objective function.
#' 
#' @examples
#' g=plotFun(a*sin(x^2)~x, x=range(-5,5),a=2)
#' g(x=1)
#' f=rfun(~u&v)
#' plotFun( f(u=u,v=v)~u&v, u=range(-3,3),v=range(-3,3) )
#' plotFun( u^2 + v < 3 ~u&v,add=TRUE,npts=200)
#' 
plotFun <- function(expr, ..., add=FALSE,
                   xlim=NULL,ylim=NULL,npts=NULL,
                   ylab=NULL, xlab=NULL, zlab=NULL, main=NULL, 
                   lwd=1,col="black",filled=TRUE,levels=NULL,nlevels=10,
                   surface=FALSE,
                   colorscheme=topo.colors,type="l",transparency=NULL ) { 
  dots <- list(...)
 
  # funny names (like ..f..) are to avoid names that might be used by the user
  # not sure whether this precaution is necessary in current implementation
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
      # graphics::lines(.xset, .yset, lwd=lwd, col=col) # base graphics
      ladd(panel.lines(.xset, .yset, lwd=lwd, col=col,...)) # lattice
	  } else { # draw a new plot
      # Base Graphics
# 	    assign("xlim", xlim2, .plotFun.envir)
# 	    graphics::plot( .xset, .yset, type=type, 
# 	                    lwd=lwd, col=col, xlim=xlim2, ylim=ylim,
# 	                    xlab=xlab,ylab=ylab,main=main)
# 	    tmp = par("usr")[c(3,4)] # get the y limits of the plot
# 	    assign("ylim", tmp, .plotFun.envir)
	   
      if( is.null(ylim)) thePlot <- lattice::xyplot(.yset ~ .xset, type=type,
                      lwd=lwd, col=col, 
					            xlim=xlim2, 
                      xlab=xlab,ylab=ylab,
					            main=main)
      else thePlot <- lattice::xyplot(.yset ~ .xset, type=type,
                                      lwd=lwd, col=col, 
                                      xlim=xlim2, ylim=ylim, 
                                      xlab=xlab,ylab=ylab,
                                      main=main)
      # goo <- par("usr") # get the limits of the plot
      # ..currentAxisLimitY <- goo[c(3,4)]
      # mosaic.par.set(currentAxisLimitX = ..currentAxisLimitX )
      # mosaic.par.set(currentAxisLimitY = ..currentAxisLimitY )
      print(thePlot)
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
    else { # Store the axis limits in memory to handle a later add=TRUE
      assign("xlim",range(xlim2),.plotFun.envir)
      assign("ylim",range(ylim2),.plotFun.envir)
    }
    
    if( (length( xlim2) < 2) & (length( xlim ) <2 ) ) {
      stop(paste("Must provide x-axis limit via ", 
                 ..f..$names, "= or xlim=", sep=""))
    }
    
    .xset <- seq(min(xlim2),max(xlim2),length=npts)
    .yset <- seq(min(ylim2),max(ylim2),length=npts)
    zvals <- outer(.xset, .yset, function(x,y){pfun(x,y)} )
    grid <- expand.grid( .xset, .yset )
    grid$height <- c(zvals)
    
    if( surface ) { 
      zcuts = pretty(grid$height,50)
      zcolors = colorscheme(length(zcuts),alpha=.5)
      if( require(manipulate) ) {
        manipulate(
          print(wireframe(height ~ Var1 + Var2, 
                          xlab=xlab,ylab=ylab,
                          zlab=list(zlab,rot=90),
                          data=grid,drape=filled,
                          shade=FALSE, colorkey=FALSE,
                          scales=list(arrows=FALSE),
                          screen=c(z=rot,x=elev-90),
                          distance=dist,
                          at = zcuts, col=rgb(1,1,1,0),
                          col.regions= zcolors
                          )),
          rot=slider(min=-180,max=180,step=5,initial=35,label="Rotation"),
          elev=slider(min=-90,max=90,step=5,initial=30,label="Elevation"),
          dist=slider(min=0,max=1,step=.01,initial=.2,label="Distance"))
      } 
      else {
        print(wireframe(height ~ Var1 + Var2, 
                        xlab=xlab,ylab=ylab,zlab=list(zlab,rot=90),
                        data=grid,drape=filled,shade=FALSE,colorkey=FALSE,
                        scales=list(arrows=FALSE),
                        col.regions= zcolors,
                        at=zcuts,
                        col=rgb(1,1,1,0)) )
      }
      
      # No ADD method yet for surface plots
    }
    else {
      # a convenience wrapper around levelplot when a contour plot
      # is being drawn de novo
      funPlot.draw.contour = function(x,y,z,ncontours=6,at=pretty(z,ncontours),
                                      filled=TRUE,color.scheme=topo.colors,
                                      labels=TRUE,contours=TRUE,
                                      col="black",lwd=1,xlab="",ylab=""){
        cal = levelplot(z~x*y,at=at, 
                        xlab=xlab,ylab=ylab, 
                        panel=panel.levelcontourplot,
                        col.regions=color.scheme(60),
                        contour=contours, labels=labels,
                        colorkey = FALSE, region = TRUE,filled=filled,
                        col=col,lwd=lwd)
        return(cal)
      }
      if( add & is.null(transparency) ) transparency<-.4
      if( is.null(transparency) ) transparency<-1
      fillcolors <- colorscheme(length(levels)+2,alpha=transparency)
      if( is.logical(zvals[1,1]) ){ # it's a constraint function
        if( add ) fillcolors<- c(rgb(0,0,0,transparency),rgb(0,0,0,0))
        else fillcolors <- colorscheme(4)
        nlevels<-2
      }
      if( add ) { # add method for contour plots
        ladd(panel.levelcontourplot(grid$Var1,grid$Var2,grid$height,
                                     subscripts=1:length(grid$Var1),
                                     at=pretty(grid$height,nlevels),
                                     col.regions=fillcolors,
                                     col=col, lwd=lwd, lty=1,
                                     filled=filled
                                     ))
      }
      else print(funPlot.draw.contour(grid$Var1,grid$Var2,grid$height, 
                           xlab=xlab,ylab=ylab,
                           filled=filled,
                           col=col,lwd=lwd))
#       if( filled) {
#         graphics::image( .xset, .yset, zvals, col=fillcolors,add=add,
#                          xlab=xlab,ylab=ylab,main=main )
#         if( is.null(levels)) levels = pretty(range(zvals),nlevels)
#         graphics::contour(.xset, .yset, zvals, col=col,lwd=lwd,
#                           add=TRUE, levels=levels,labcex=1.05,vfont=NULL)
#       }
#       else {
#         graphics::contour(.xset,.yset,zvals,levels=levels,add=add,lwd=lwd,col=col,
#                           xlab=xlab,ylab=ylab,main=main)
#       } 
    }
  }
  else if( ndims > 2 ) 
    stop("More than 2 plotting variables.")
  
  invisible(..f..$fun)
}
# =============================
# Create an environment for storing axis limits, etc.
.plotFun.envir = new.env(parent=baseenv())
# =====
#' 
#' Lattice plot that draws a filled contour plot
#' 
#' Used within plotFun
#'
#' @param x x on a grid
#' @param y y on a grid
#' @param z zvalues for the x and y
#' @param subscripts which points to plot
#' @param at cuts for the contours
#' @param shrink what does this do?
#' @param labels draw the contour labels
#' @param label.style where to put the labels
#' @param contour logical draw the contours
#' @param region logical color the regions
#' @param col color for contours
#' @param lty type for contours
#' @param lwd width for contour
#' @param border type of border
#' @param \ldots additional arguments
#' @param col.regions color set for regions
#' @param color.scheme color generation function, e.g. \code{rainbow}
#' @param filled whether to fill the contours with color
#' @param alpha.regions transparency of regions
panel.levelcontourplot = function(x, y, z, subscripts, 
                                   at, shrink, labels = FALSE, 
                                   label.style = c("mixed","flat","align"), 
                                   contour = FALSE, 
                                   region = TRUE,
                                   col = add.line$col, lty = add.line$lty,
                                   lwd = add.line$lwd, 
                                   border = "transparent", ...,
                                   col.regions = regions$col,
                                   color.scheme, filled=TRUE, 
                                   alpha.regions = regions$alpha
                                   ){
  if(filled) panel.levelplot(x, y, z, subscripts, 
                             at = pretty(z,5*length(at)), shrink, 
                             labels = FALSE, 
                             label.style = label.style, 
                             contour = FALSE, 
                             region = TRUE, 
                             border = "transparent", ..., 
                             col.regions = col.regions #, 
                             #                  alpha.regions = regions$alpha
                             )
  panel.levelplot(x, y, z, subscripts, 
                  at = at, shrink, labels = TRUE, 
                  label.style = label.style, 
                  contour = TRUE, 
                  region = FALSE, 
                  col = col, lwd=lwd,
                  border = "transparent", ...)
}
