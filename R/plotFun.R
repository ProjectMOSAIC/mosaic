#' Plotting mathematical expressions
#'
#' Plots mathematical expressions in one and two variables.  
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @rdname plotFun
#' @name plotFun
#' @aliases plotFun
#'
#' @param object a mathematical expression (see examples)
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
#' plotFun(a*sin(x^2)~x, x=range(-5,5), a=2)
#' f <- rfun( ~ u & v)
#' plotFun( f(u=u,v=v) ~ u & v, u=range(-3,3), v=range(-3,3) )
#' plotFun( u^2 + v < 3 ~ u & v, add=TRUE, npts=200)
#' 
plotFun <- function(object, ..., add=FALSE,
					xlim=NULL, ylim=NULL, npts=NULL,
					ylab=NULL, xlab=NULL, zlab=NULL, main=NULL, 
					lwd=1,col="black", filled=TRUE, 
					levels=NULL, nlevels=10,
					surface=FALSE,
					colorscheme=topo.colors, type="l", 
					transparency=NULL ) { 

	if (add) { 
		ladd( panel.plotFun( object, npts=npts, lwd=lwd, col=col, 
							filled=filled, levels=levels, nlevels=nlevels, surface=surface, 
							colorscheme=colorscheme, type=type, transparency=transparency, ...))
		return(invisible(NULL))
	} 

	dots <- list(...)

	# funny names (like ..f..) are to avoid names that might be used by the user
	# not sure whether this precaution is necessary in current implementation

	..f.. <- makeFunction( object )

	vars <- formals(..f..)
	rhsVars <- all.vars(rhs(object))
	ndims <- length(rhsVars)

	if( ndims == 1 ){

		npts <- ifelse( is.null(npts), 200, npts)
		# create a function of that one variable
		pfun <- function(x){  # removed . from name, was .x
			mydots <- dots
			mydots[[rhsVars]] <- x
			eval( lhs(object), envir=mydots, enclos=parent.frame() )
		}

		# Set the axis labels
		# deparse needed for lattice (not originally for plot)

		if( is.null(ylab) ) ylab <- deparse( lhs(object) ) # deparse(..f..$sexpr)
		if( is.null(xlab) ) xlab <- rhsVars

		# figure out the limits.  
		# Is a limit specified, either through xlim or the variable name
		xlim2 <- xlim
		if( rhsVars %in% names(dots) ) {
			xlim2 <- range(dots[[rhsVars]])
		}

		if( length(xlim2) != 2 ) { # no limits were specified
			xlim2 <- c(0,1)   # temporary default -- should use roots and critical values instead
		}

		if( (length( xlim2) != 2) ) {
			stop(paste("Must provide x-axis limit via ", 
					   rhsVars, "= or xlim=", sep=""))
		}



		# Evaluate the function on appropriate inputs.
		.xset <- mosaic::adapt_seq(min(xlim2), max(xlim2), 
								   f=function(xxqq){ pfun(xxqq) }, length=npts)
		.yset <- sapply( .xset, pfun )  # pfun(.xset)

		if( is.null(ylim)) {
			thePlot <- lattice::xyplot(.yset ~ .xset, type=type,
									   lwd=lwd, col=col, 
									   xlim=xlim2, 
									   xlab=xlab, ylab=ylab,
									   main=main,
									   panel=panel.xyplot
									   # object=object
									   )
		} else { 
			thePlot <- lattice::xyplot(.yset ~ .xset, type=type,
									   lwd=lwd, col=col, 
									   xlim=xlim2, ylim=ylim, 
									   xlab=xlab,ylab=ylab,
									   main=main,
									   panel=panel.xyplot
									   #object=object
									   )
		}
		return(thePlot)
	}  # end ndims == 1

	if (ndims == 2 ) {
		npts <- ifelse( is.null(npts), 40, npts)
		# create a function of those two variables
		pfun <- function(.x,.y){
			dots[[rhsVars[1]]] <- .x
			dots[[rhsVars[2]]] <- .y
			eval( lhs(object), envir=dots, enclos=parent.frame())
		}
		if( length(ylab) == 0 ) ylab <- rhsVars[2]
		if( length(xlab) == 0 ) xlab <- rhsVars[1]
		if( length(zlab) == 0 ) zlab <- deparse(lhs(object))
		xlim2 <- xlim
		ylim2 <- ylim
		if( rhsVars[1] %in% names(dots) ) {
			xlim2 <- range(dots[[rhsVars[1]]])
		}
		if( rhsVars[2] %in% names(dots) ) {
			ylim2 <- range(dots[[rhsVars[2]]])
		}

		if( length(xlim2) != 2 ) { # no limits were specified
			xlim2 <- c(0,1)   # temporary default 
		}
		if( length(ylim2) != 2 ) { # no limits were specified
			ylim2 <- c(0,1)   # temporary default
		}

		.xset <- seq(min(xlim2),max(xlim2),length=npts)
		.yset <- seq(min(ylim2),max(ylim2),length=npts)
		zvals <- outer(.xset, .yset, function(x,y){pfun(x,y)} )
		grid <- expand.grid( .xset, .yset )
		grid$height <- c(zvals)

		if( surface ) { 
			if (add) {
				stop('Should not get here, but no add option for surface plots yet anyway.')
				return(invisible(NULL))
			}
			zcuts = pretty(grid$height,50)
			zcolors = colorscheme(length(zcuts),alpha=.5)
			if( FALSE && require(manipulate) ) {
				manipulate(
						   return(wireframe(height ~ Var1 + Var2, 
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
			} else {  # without manipulate
				return(wireframe(height ~ Var1 + Var2, 
								 xlab=xlab,ylab=ylab,zlab=list(zlab,rot=90),
								 data=grid,drape=filled,shade=FALSE,colorkey=FALSE,
								 scales=list(arrows=FALSE),
								 col.regions= zcolors,
								 at=zcuts,
								 col=rgb(1,1,1,0)) )
			}

		} else {  # i.e., surface==FALSE
			# a convenience wrapper around levelplot when a contour plot
			# is being drawn de novo
			funPlot.draw.contour <- function(x,y,z,ncontours=6,at=pretty(z,ncontours),
											 filled=TRUE,color.scheme=topo.colors,
											 labels=TRUE,contours=TRUE,
											 col="black",lwd=1,xlab="",ylab="",...){
				return(levelplot(z~x*y, at=at, 
								 xlab=xlab, ylab=ylab, 
								 panel=panel.levelcontourplot,
								 col.regions=color.scheme(60),
								 contour=contours, labels=labels,
								 colorkey = FALSE, region = TRUE, filled=filled,
								 col=col, lwd=lwd, ...)
				)
			}

			if( is.null(transparency) ) transparency <- 1
			fillcolors <- colorscheme(length(levels)+2, alpha=transparency)

			if( all (is.logical(zvals) ) ){  # it's a constraint function
					fillcolors <- colorscheme(4, alpha=transparency)
				nlevels <- 2
			}

			return(funPlot.draw.contour(grid$Var1, grid$Var2, grid$height, 
											 xlab=xlab, ylab=ylab,
											 filled=filled,
											 col=col, lwd=lwd, ...))
		}
	}
	stop("Bug alert: You should not get here.  Please report.")
}

#' Panel function for plotting functions
#'
#' @seealso plotFun
#' @param object an object (e.g., a formula) describing a function
#' @param \dots additional arguments passed on to other panel functions
#' @param npts an integer giving the number of points (in each dimension) to sample the function
#' @param zlab label for z axis (when in surface-plot mode)
#' @param lwd line width for line graphs and contours
#' @param col color for line graphs and contours
#' @param filled fill with color between the contours (\code{TRUE} by default)
#' @param levels levels at which to draw contours
#' @param nlevels number of contours to draw (if \code{levels} not specified)
#' @param surface a logical indicating whether to draw a surface plot rather than a contour plot
#' @param colorscheme a function (\code{topo.colors} by default) for choosing colors for fill
#' @param type type of plot (\code{"l"} by default)
#' @param transparency number from 0 (transparent) to 1 (opaque) for the fill colors 
#'
#' @examples
#' x <- runif(30,0,2*pi) 
#' d <- data.frame( x = x,  y = sin(x) + rnorm(30,sd=.2) )
#' xyplot( y ~ x, data=d )
#' ladd(panel.plotFun( sin(x) ~ x ) )

panel.plotFun <- function( object, ..., 
                   npts=NULL,
                   zlab=NULL, 
                   lwd=1, col="black", filled=TRUE, 
				   levels=NULL, nlevels=10,
                   surface=FALSE,
                   colorscheme=topo.colors, type="l", 
				   transparency=NULL ) { 
  dots <- list(...)
 
  # funny names (like ..f..) are to avoid names that might be used by the user
  # not sure whether this precaution is necessary in current implementation
  
  ..f.. <- makeFunction( object )

  vars <- formals(..f..)
  rhsVars <- all.vars(rhs(object))
  ndims <- length(rhsVars)
 
  parent.xlim <- current.panel.limits()$xlim
  parent.ylim <- current.panel.limits()$ylim

  if( ndims > 2 || ndims < 1 ) 
    stop("Formula must provide 1 or 2 independent variables (right hand side).")

  if( ndims == 1 ){
    npts <- ifelse( is.null(npts), 200, npts)
    # create a function of that one variable
    # this should probably by refactorred into a new function
    pfun <- function(x){  # removed . from name, was .x
	    mydots <- dots
      mydots[[rhsVars]] <- x
      eval( lhs(object), envir=mydots, enclos=parent.frame())
    }

    # Evaluate the function on appropriate inputs.
	.xset <- mosaic::adapt_seq(min(parent.xlim), max(parent.xlim), 
				f=function(xxqq){ pfun(xxqq) }, length=npts)
  	.yset <- sapply( .xset, pfun )  # pfun(.xset)

	return(panel.xyplot(.xset, .yset, lwd=lwd, col=col, type=type, ...)) 
  }
	   
  if (ndims == 2 ) {
    if( surface ) { 
			stop('no add option for surface plots yet.')
			return(NULL)
	}

    # if we get here, surface == FALSE & ndims=2
    npts <- ifelse( is.null(npts), 40, npts)
    # create a function of those two variables
    pfun <- function(.x,.y){
      dots[[rhsVars[1]]] <- .x
      dots[[rhsVars[2]]] <- .y
      eval( lhs(object), envir=dots, enclos=parent.frame() )
    }

    if( length(zlab) == 0 ) zlab <- deparse(lhs(object) )
    
    .xset <- seq(min(parent.xlim),max(parent.xlim),length=npts)
    .yset <- seq(min(parent.ylim),max(parent.ylim),length=npts)
    zvals <- outer(.xset, .yset, function(x,y){pfun(x,y)} )
    grid <- expand.grid( .xset, .yset )
    grid$height <- c(zvals)
    
	zcuts = pretty(grid$height,50)
	zcolors = colorscheme(length(zcuts),alpha=.5)
	if( is.null(transparency) ) transparency<-.4
	fillcolors <- colorscheme(length(levels) + 2, alpha=transparency)

	if( all(is.logical(zvals)) ) {  # it's a constraint function
		# fillcolors <- c(rgb(0,0,0,transparency), rgb(0,0,0,0))
		fillcolors <- colorscheme(4, transparency)
		nlevels <- 2
	}

	return( panel.levelcontourplot(x = grid$Var1, y = grid$Var2, z = grid$height,
						   subscripts = 1:nrow(grid),
						   at = pretty(grid$height,nlevels),
						   col.regions = fillcolors,
						   col=col, lwd = lwd, lty = 1,
						   filled=filled, ...
						   )
	)
   
  }
  stop("Bug alert: You should not get here.  Please report.")
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

panel.levelcontourplot <- function(x, y, z, subscripts, 
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
