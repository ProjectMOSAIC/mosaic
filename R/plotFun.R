#' Plotting mathematical expressions
#'
#' Plots mathematical expressions in one and two variables.  
#'
#' @rdname plotFun
#' @name plotFun
#' @aliases plotFun
#'
#' @param object a mathematical expression (see examples)
#' @param add if TRUE, then overlay an existing plot
#' @param xlim limits for x axis (or use variable names, see examples)
#' @param ylim limits for y axis (or use variable names, see examples)
#' @param npts number of points for plotting. 
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param zlab label for z axis (when in surface-plot mode)
#' @param col color for line graphs and contours
#' @param filled fill with color between the contours (\code{TRUE} by default)
#' @param levels levels at which to draw contours
#' @param nlevels number of contours to draw (if \code{levels} not specified)
#' @param surface draw a surface plot rather than a contour plot
#' @param col.regions  a vector of colors or a function (\code{topo.colors} by default) for generating such
#' @param type type of plot (\code{"l"} by default)
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
#' Additionally, these arguments can be used to specify parameters for the function being 
#' plotted and to specify the plotting window with natural names.  See the examples for such usage.
#'
#' @return a \code{trellis} object
#'
#' @details
#' makes plots of mathematical expressions using the formula syntax.  Will
#' draw both line plots and contour/surface plots (for functions of two variables).
#' In RStudio, the surface plot comes with sliders to set orientation.
#' If the colors in filled surface plots are too blocky, increase \code{npts} 
#' beyond the default of 50, though \code{npts=300} is as much as you're likely to ever need.
#' See examples for overplotting a constraint function on an objective function.
#' 
#' @examples
#' plotFun( a*sin(x^2)~x, xlim=range(-5,5), a=2 )  # setting parameter value
#' plotFun( u^2 ~ u, ulim=c(-4,4) )                # limits in terms of u
#' # Note roles of ylim and y.lim in this example
#' plotFun( y^2 ~ y, ylim=c(-2,20), y.lim=c(-4,4) )    
#' # Combining plot elements to show the solution to an inequality
#' plotFun( x^2 -3 ~ x, xlim=c(-4,4), grid=TRUE )
#' ladd( panel.abline(h=0,v=0,col='gray50') )
#' plotFun( (x^2 -3) * (x^2 > 3) ~ x, type='h', alpha=.1, lwd=4, col='lightblue', add=TRUE )
#' plotFun( sin(x) ~ x, 
#'    groups=cut(x, findZeros(sin(x) ~ x, within=10)), 
#'    col=c('blue','green'), lty=2, lwd=3, xlim=c(-10,10) )
#' f <- rfun( ~ u & v )
#' plotFun( f(u=u,v=v) ~ u & v, u.lim=range(-3,3), v.lim=range(-3,3) )
#' plotFun( u^2 + v < 3 ~ u & v, add=TRUE, npts=200 )
#' # display a linear model using a formula interface
#' model <- lm(wage ~ poly(exper,degree=2), data=CPS)
#' fit <- makeFun(model)
#' xyplot(wage ~ exper, data=CPS)
#' plotFun(fit(exper) ~ exper, add=TRUE)
 
plotFun <- function(object, ..., add=FALSE,
					xlim=NULL, ylim=NULL, npts=NULL,
					ylab=NULL, xlab=NULL, zlab=NULL, 
					filled=TRUE, 
					levels=NULL, nlevels=10,
					surface=FALSE,
					groups=NULL,
					col.regions=topo.colors, 
					type="l", 
					alpha=NULL ) { 
	
	if ( is.vector(col.regions ) ) col.regions  <- makeColorscheme(col.regions )

	if (add) { 
		ladd( panel.plotFun( object, npts=npts, # lwd=lwd, #col=col, 
							filled=filled, levels=levels, nlevels=nlevels, surface=surface, 
							col.regions=col.regions, type=type, alpha=alpha, ...))
		return(invisible(NULL))
	} 

	dots <- list(...)
	dots[['type']] <- type
	dots[['alpha']] <- alpha

	# funny names (like ..f..) are to avoid names that might be used by the user
	# not sure whether this precaution is necessary in current implementation

	# perhaps use environment(object)?
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

	if( ndims == 1 ){

		npts <- ifelse( is.null(npts), 200, npts)

		# Set the axis labels
		# deparse needed for lattice (not originally for plot)

		if( is.null(ylab) ) ylab <- deparse( lhs(object) ) # deparse(..f..$sexpr)
		if( is.null(xlab) ) xlab <- rhsVars

		if (is.null(limits$xlim) || length(limits$xlim) < 2 ) {
			zeros <- findZeros( object, nearest=6 )
			limits$xlim <- switch(as.character(length(zeros)), 
				"0" = c(0,1),
				"1" = c(-1.5,1.5) * zeros,
				( c(-.1,.1) * diff(range(zeros)) ) + range(zeros)
			)
		} 

		limits$xlim <- range(limits$xlim)

		if (length(limits$xlim) !=2) stop ("Invalid limits.")

		# Evaluate the function on appropriate inputs.
		.xvals <- 
			if ('h' %in% type)  
				seq(min(limits$xlim), max(limits$xlim), length.out=npts)
			else 
				mosaic::adapt_seq(min(limits$xlim), max(limits$xlim), 
										   f=function(xxqq){ ..f..(xxqq) }, length=npts)
		
		.yvals <- sapply( .xvals, ..f.. )  # pfun(.xvals)
		localData <- data.frame(.xvals, .yvals, .xvals, .yvals)
		names(localData) <- c("x", "y", rhsVars, paste('f(',rhsVars,')',sep=''))

		# note: passing ... through to the lattice functions currently conflicts with
		# using ... to set values of "co-variates" of the function.
		# print(cleanDots)
		if( length(limits$ylim) != 2 ) {
			thePlot <- do.call(lattice::xyplot,
							 c(list(y ~ x, 
								data=localData,
								# groups=eval(substitute(groups),localData),
								groups=substitute(groups),
								xlim=limits$xlim, 
								xlab=xlab, ylab=ylab,
								panel=panel.xyplot
								),
								cleanDots
								)
			)
		} else { 
			thePlot <- do.call(lattice::xyplot, c(list(
							    y ~ x,
								data=localData,
								# groups=eval(substitute(groups),localData),
								groups=substitute(groups),
								xlim=limits$xlim, 
								ylim=limits$ylim, 
								xlab=xlab,ylab=ylab,
								panel=panel.xyplot),
								cleanDots)
								)
		}
		return(thePlot)
	}  # end ndims == 1

	if (ndims == 2 ) {
		npts <- ifelse( is.null(npts), 40, npts)
		if( length(ylab) == 0 ) ylab <- rhsVars[2]
		if( length(xlab) == 0 ) xlab <- rhsVars[1]
		if( length(zlab) == 0 ) zlab <- deparse(lhs(object))

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
		zvals <- outer(.xvals, .yvals, function(x,y){..f..(x,y)} )
		grid <- expand.grid( .xvals, .yvals )
		grid$height <- c(zvals)
		localData <- grid
		names(localData) <- c(rhsVars, '.height')

		if( surface ) { 
			if (add) {
				stop('Should not get here, but no add option for surface plots yet anyway.')
				return(invisible(NULL))
			}
			zcuts = pretty(grid$height,50)
			zcolors = col.regions (length(zcuts),alpha=.5*alpha)
			if( require(manipulate) ) {
				return(manipulate(
						   wireframe(height ~ Var1 + Var2, 
											xlab=xlab,ylab=ylab,
											zlab=list(zlab,rot=90),
											#data=localData, 
											data=grid,
											groups = eval(substitute(groups), localData),
											drape=filled,
											shade=FALSE, colorkey=FALSE,
											scales=list(arrows=FALSE),
											screen=c(z=rot,x=elev-90),
											distance=dist,
											at = zcuts, #col=rgb(1,1,1,0),
											col.regions= zcolors
											#...
											),
						   rot=slider(min=-180,max=180,step=5,initial=35,label="Rotation"),
						   elev=slider(min=-90,max=90,step=5,initial=30,label="Elevation"),
						   dist=slider(min=0,max=1,step=.01,initial=.2,label="Distance")
						   ))
			} else {  # without manipulate
				return(wireframe(height ~ Var1 + Var2, 
								 xlab=xlab,ylab=ylab,zlab=list(zlab,rot=90),
								 data=grid,
								 groups=eval(substitute(groups), localData),
								 drape=filled,shade=FALSE,colorkey=FALSE,
								 scales=list(arrows=FALSE),
								 col.regions= zcolors,
								 # col=rgb(1,1,1,0),
								 at=zcuts
								 ) )
			}

		} else {  # i.e., surface==FALSE
			# a convenience wrapper around levelplot when a contour plot
			# is being drawn de novo
			funPlot.draw.contour <- function(x,y,z,ncontours=6,at=pretty(z,ncontours),
											 filled=TRUE, col.regions=topo.colors,
											 labels=TRUE, contours=TRUE, groups=NULL,
											 xlab="",ylab="", ...){
				return(levelplot(z~x*y, at=at, 
								 xlab=xlab, ylab=ylab, 
								 panel=panel.levelcontourplot,
								 groups=substitute(groups),
								 col.regions=col.regions(60),
								 contour=contours, labels=labels,
								 colorkey = FALSE, region = TRUE, filled=filled,
								 #col=col, 
								 ...) 
				)
			}

			if( is.null(alpha) ) alpha <- 1
			fillcolors <- col.regions (length(levels)+2, alpha=alpha)

			if( all (is.logical(zvals) ) ){  # it's a constraint function
					fillcolors <- col.regions (4, alpha=alpha)
				nlevels <- 2
			}

			return(funPlot.draw.contour(grid$Var1, grid$Var2, grid$height, 
											 xlab=xlab, ylab=ylab,
											 filled=filled,
											 groups=eval(substitute(groups),localData),
											 col.regions = col.regions ,
											 #col=col, 
											 ...) )
		}
	}
	stop("Bug alert: You should not get here.  Please report.")
}

#' Panel function for plotting functions
#'
#' @seealso plotFun
#' @param object an object (e.g., a formula) describing a function
#' @param npts an integer giving the number of points (in each dimension) to sample the function
#' @param zlab label for z axis (when in surface-plot mode)
#' @param filled fill with color between the contours (\code{TRUE} by default)
#' @param levels levels at which to draw contours
#' @param nlevels number of contours to draw (if \code{levels} not specified)
#' @param surface a logical indicating whether to draw a surface plot rather than a contour plot
#' @param col.regions  a vector of colors or a function (\code{topo.colors} by default) for generating such
#' @param type type of plot (\code{"l"} by default)
#' @param alpha number from 0 (transparent) to 1 (opaque) for the fill colors 
#' @param \dots additional arguments, typically processed by \code{lattice} panel functions
#'        such as \code{\link[lattice]{panel.xyplot}} or \code{\link[lattice]{panel.levelplot}}.
#'        Frequently used arguments include
#'        \describe{
#'        	\item{\code{lwd}}{line width}
#'        	\item{\code{lty}}{line type}
#'        	\item{\code{col}}{a color}
#'        }
#'
#' @examples
#' x <- runif(30,0,2*pi) 
#' d <- data.frame( x = x,  y = sin(x) + rnorm(30,sd=.2) )
#' xyplot( y ~ x, data=d )
#' ladd(panel.plotFun( sin(x) ~ x, col='red' ) )
#' xyplot( y ~ x | rbinom(30,1,.5), data=d )
#' ladd(panel.plotFun( sin(x) ~ x, col='red', lty=2 ) )    # plots sin(x) in each panel

panel.plotFun <- function( object, ..., 
						  type="l", 
						  npts=NULL,
						  zlab=NULL, 
						  filled=TRUE, 
						  levels=NULL, 
						  nlevels=10,
						  surface=FALSE,
                   		  col.regions =topo.colors, 
				   		  alpha=NULL ) { 
  dots <- list(...)
  if ( is.vector(col.regions ) ) col.regions  <- makeColorscheme(col.regions )

  plot.line <- trellis.par.get('plot.line')
  superpose.line <- trellis.par.get('superpose.line')
 
  # funny names (like ..f..) are to avoid names that might be used by the user
  # not sure whether this precaution is necessary in current implementation
 
  # perhaps use environment(object)?
  ..f.. <- do.call( "makeFun", c(object, dots, strict.declaration=FALSE), envir= parent.frame())  

  vars <- formals(..f..)
  rhsVars <- all.vars(rhs(object))
  ndims <- length(rhsVars)
 
  parent.xlim <- current.panel.limits()$xlim
  parent.ylim <- current.panel.limits()$ylim

  if( ndims > 2 || ndims < 1 ) 
    stop("Formula must provide 1 or 2 independent variables (right hand side).")

  if( ndims == 1 ){
	  npts <- ifelse( is.null(npts), 200, npts)

	  # Evaluate the function on appropriate inputs.
	  .xvals <- 
		  if ('h' %in% type)  
			  seq(min(parent.xlim), max(parent.xlim), length.out=npts)
			  else 
				  mosaic::adapt_seq(min(parent.xlim), max(parent.xlim), 
									f=function(xxqq){ ..f..(xxqq) }, length=npts)
	  .yvals <- sapply( .xvals, ..f.. )  # pfun(.xvals)

	  return(panel.xyplot(.xvals, .yvals, type=type, alpha=alpha, ...))
	  #return(panel.xyplot(.xvals, .yvals, ...))
  }
	   
  if (ndims == 2 ) {
    if( surface ) { 
			stop('no add option for surface plots yet.')
			return(NULL)
	}
    # if we get here, surface == FALSE & ndims=2
    npts <- ifelse( is.null(npts), 40, npts)
    # create a function of those two variables

    if( length(zlab) == 0 ) zlab <- deparse(lhs(object) )
    
    .xvals <- seq(min(parent.xlim),max(parent.xlim),length=npts)
    .yvals <- seq(min(parent.ylim),max(parent.ylim),length=npts)
    zvals <- outer(.xvals, .yvals, function(x,y){..f..(x,y)} )
    grid <- expand.grid( .xvals, .yvals )
    grid$height <- c(zvals)
    
	zcuts <- pretty(grid$height,50)
	zcolors <- col.regions (length(zcuts),alpha=.5)
	# print(zcolors)
	if( is.null(alpha) ) alpha<-.4

	if( all(is.logical(zvals)) ) {  # it's a constraint function
		nlevels <- 2
	}
	fillcolors <- col.regions (length(levels) + 2, alpha=alpha)

	return( panel.levelcontourplot(x = grid$Var1, y = grid$Var2, z = grid$height,
						   subscripts = 1:nrow(grid),
						   at = pretty(grid$height,nlevels),
						   col.regions = fillcolors,
						   filled=filled,
						   ...
						   #col=col, lwd = lwd, lty = 1,
						   )
	)
  }
  stop("Bug alert: You should not get here.  Please report.")
}



#' Infer arguments 
#' 
#' The primary purpose is for inferring argument settings from names derived from variables
#' occurring in a formula.  For example, the default use is to infer limits for variables
#' without having to call them \code{xlim} and \code{ylim} when the variables in the formula
#' have other names.  Other uses could easily be devised by specifying different \code{variants}.
#' 
#' @param vars a vector of variable names to look for
#' @param dots a named list of argument values
#' @param defaults named list or alist of default values for limits
#' @param variants a vector of optional postfixes for limit-specifying variable names
#' @return a named list or alist of limits.  The names are determined by the names in \code{defaults}.
#'
#' If multiple \code{variants} are matched, the first is used.
#' @examples
#' inferArgs(c('x','u','t'), list(t=c(1,3), x.lim=c(1,10), u=c(1,3), u.lim=c(2,4)))
#' inferArgs(c('x','u'), list(u=c(1,3)), defaults=list(xlim=c(0,1), ylim=NULL)) 


inferArgs <- function( vars, dots, defaults=alist(xlim=, ylim=, zlim=), variants=c('.lim','lim') ) {
	limNames <- names(defaults)
	if (length(vars) > length(limNames)) 
		stop( paste("Can only infer", 
					paste(limNames, sep=""), 
					"; but you provided", 
					length(vars)),
			 		" variables to search for.", 
					sep="")
	result <-defaults

	for (slot in 1:length(vars)) {
		for (variant in rev(variants)){
			var.lim <- paste(vars[slot],variant,sep="")
			if (! is.null(dots[[var.lim]]) ) {
				result[[limNames[slot]]] <- dots[[var.lim]]
			}
		}
	}
	return(result)
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
#' @param \dots additional arguments
#' @param col.regions  a vector of colors or a function (\code{topo.colors} by default) for generating such
#' @param filled whether to fill the contours with color
#' @param alpha.regions transparency of regions

panel.levelcontourplot <- function(x, y, z, subscripts, 
                                   at, shrink, labels = FALSE, 
                                   label.style = c("mixed","flat","align"), 
                                   contour = FALSE, 
                                   region = TRUE,
                                   col = add.line$col, 
								   lty = add.line$lty,
                                   lwd = add.line$lwd, 
                                   border = "transparent", ...,
                                   col.regions = regions$col,
                                   filled=TRUE, 
                                   alpha.regions = regions$alpha
                                   ){
	add.line <- trellis.par.get('add.line')
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

#' Create a color generating function from a vector of colors
#'
#' @param col a vector of colors
#' @return a function that generates a vector of colors interpolated among the colors in \code{col}
#'
#' @export
#' @examples
#' cs <- makeColorscheme( c('red','white','blue') )
#' cs(10)
#' cs(10, alpha=.5)

makeColorscheme <- function(col) {
	result <- function(n, alpha=1, ...)  {
		idx <-  0.5 + (0:n)/(n+0.001) * (length(colorList))  
		return( apply(col2rgb(col[ round(idx) ]), 2, 
					  function(x) { rgb(x[1],x[2],x[3], alpha=round(alpha*255), maxColorValue=255) } ) )
	}
	e <- new.env()
	e[['colorList']] <- col
	environment(result) <- e
	return(result)
}
