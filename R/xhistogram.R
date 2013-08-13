#
# this appears to be unused
#
#.drop_from_list <- function( l, names ) {
#	for (n in names) {
#		if (n %in% names(l) ) {
#			dots[[n]] <- NULL
#		}
#	}
#}

#' Augmented histograms
#' 
#' The \pkg{mosaic} \code{histogram} adds some additional functionality to 
#' \code{\link[lattice]{histogram}} making it simpler to obtain certain common 
#' histogram adornments.
#' @rdname xhistogram
#'
#' @param x a formula or a numeric vector
#' @param data a data frame in which to evaluate \code{x}
#' @param panel a panel function
#' @param type one of \code{'density'}, \code{'count'}, or \code{'percent'}
#' @param nint approximate number of bins
#' @param breaks break points for histogram bins, a function for computing such,
#'        or a method \code{\link{hist}} knows about given as a character string.
#'        By default, \code{\link[mosaic]{xhistogramBreaks}} is used.
#'        
#' @param \dots additional arguments passed to \code{\link[lattice]{histogram}} and (by
#' default when the \pkg{mosaic} package has been loaded) on to 
#' \code{\link{panel.xhistogram}}
#'
#' @return a trellis object
#' @seealso \code{\link[lattice]{histogram}}, \code{xhistogramBreaks}
#' 
#' @export
#' @examples
#' histogram(~age | substance, HELPrct, v=35, fit='normal')
#' histogram(~age, HELPrct, labels=TRUE, type='count')
#' histogram(~age, HELPrct, groups=cut(age, seq(10,80,by=10)))
#' histogram(~age, HELPrct, groups=sex, stripes='horizontal')
#' histogram(~racegrp, HELPrct, groups=substance,auto.key=TRUE)

#histogram <- function( x, data, panel=lattice.getOption('panel.histogram'), 
#                       breaks=xhistogramBreaks, ... ) {
#  lattice::histogram( x, data, panel=panel, breaks=breaks, ...)
#}

#' @export
histogram <- function (x, data=NULL, panel=panel.xhistogram, type='density', 
						center=NULL, width=NULL, breaks, nint, ...) {
	if ( missing(breaks) ) {
		if (inherits(x,"formula")) {
			if (is.null(data)) {
				xvals <- eval(rhs(x), parent.frame())
			} else {
				xvals <- eval(rhs(x), data)
			}
		} else {
			xvals <- x
		}
		breaks <- xhistogramBreaks(xvals, center=center, width=width, nint=nint)
	}

	lattice::histogram(x, data=data, panel=panel, type=type, center=center, width=width, 
			  nint=substitute(nint), breaks=breaks, ...)
}

#prepanel.mosaic.histogram <- function (x, breaks, ...) 
#{
#  if (length(x) < 1) 
#    return(prepanel.null())
#  
#	if ( missing(breaks) ) {
#		  breaks <- xhistogramBreaks(x, ...)
#	} else {
#    print (breaks)
#	}
#  lattice::prepanel.default.histogram( x, breaks, ... )
#}


#' @rdname xhistogram
#' @export
xhistogram <- function (x, data=NULL, panel=panel.xhistogram, type='density', 
                                      center=NULL, width=NULL, ...) {
   .Deprecated("histogram")
   histogram(x, data=data, panel=panel, type=type, center=center, 
             width=width, ...)
 }

#' @rdname xhistogram
#' @return \code{xhistogramBreaks} returns a vector of break points
#' @examples
#' xhistogramBreaks(1:10, center=5, width=1)
#' xhistogramBreaks(1:10, center=5, width=2)
#' xhistogramBreaks(0:10, center=15, width=3)
#' xhistogramBreaks(1:100, center=50, width=3)
#' xhistogramBreaks(0:10, center=5, nint=5)

xhistogramBreaks <- function(x, center=NULL, width=NULL, nint, ...) {
  x <- x[!is.na(x)]
  if (is.factor(x)) return(seq_len(1 + nlevels(x)) - 0.5)
  if (length(x) < 2) return (x)
  
  if (is.null(center)) { center <- 0 }
  if (missing(nint) || is.null(nint)) { 
    nint <- round(1.5 *log2(length(x)) + 1) 
  }
  if (is.null(width)) { width <- diff(range(x)) / nint }

  shift <- -.5 + ( (floor( (min(x) - center)/width) ):(1 + ceiling( (max(x) - center)/width)) )
  breaks <-  center + shift * width
  if (min(breaks) > min(x) || max(breaks) < max(x)) 
	  stop("Bug alert: break points don't cover data.")
  return(breaks)
}

#' @rdname xhistogram
#' @param dcol color of density curve
#' @param gcol color of guidelines
#' @param fcol fill color for histogram rectangles
#' @param dmath density function for density curve overlay
#' @param verbose be verbose?
#' @param dn number of points to sample from density curve
#' @param dlwd,glwd like \code{lwd} but affecting the density line and guide lines, respectively
#' @param args a list of additional arguments for \code{dmath}
#' @param labels should counts/densities/precents be displayed or each bin?
#' @param density overlay density?
#' @param fit a character string describing the distribution to fit.  Known distributions include
#'      \code{"exponential"}, \code{"normal"}, \code{"lognormal" }, \code{"poisson"}, \code{"beta"}, \code{"geometric"},
#'      \code{"t"}, \code{"weibull"}, \code{"cauchy"}, \code{"gamma"}, \code{"chisq"}, and \code{"chi-squared"}
#'        
#' @param start numeric value passed to \code{\link[MASS]{fitdistr}}
#' @param center center of one of the bins
#' @param width width of the bins
#' @param groups as per \code{\link[lattice]{histogram}}
#' @param stripes one of \code{"vertical"}, \code{"horizontal"}, or \code{"none"}, indicating
#'        how bins should be striped when \code{groups} is not \code{NULL}
#' @param h,v a vector of values for additional horizontal and vertical lines
#' @param alpha transparency level
panel.xhistogram <-
function (x, 
	dcol = trellis.par.get("plot.line")$col, dlwd = 2, 
    gcol = trellis.par.get("add.line")$col, glwd = 2, 
	fcol = trellis.par.get("superpose.polygon")$col,
	dmath = dnorm, 
	verbose = FALSE,
    dn = 100, args = NULL, labels = FALSE, density = FALSE, fit = NULL, 
    start = NULL, type = "density", v, h, groups=NULL, center=NULL, width=NULL, breaks,
    nint = round(1.5 * log2(length(x)) + 1),
	stripes=c('vertical','horizontal','none'), alpha=1, ...) 
{
	if (missing(breaks) || is.null(breaks)) {
    breaks <- xhistogramBreaks(x, center=center, width=width, nint=nint)
	} 
  stripes <- match.arg(stripes)
	if (!is.null(groups)) {
    	hist.master <- hist(as.numeric(x), plot = FALSE, breaks=breaks, warn.unused=FALSE, ...)
		hist.master$height <- switch(type,
			'density' = hist.master$density,
			'count' = hist.master$count,
			'percent' = 100 * hist.master$count / length(x)
			)
		nbreaks <- length(hist.master$breaks)
		groups <- factor(groups)
		ngroups <- nlevels(groups)
		props <- (table(groups))/length(groups)
		fcol <- rep(fcol, length=length(props))
		cumdensity= rep(0, length(hist.master$mids))
		cumrdensity= rep(0, length(hist.master$mids))
		for (level in 1:ngroups) {
			hist.level <- hist(
				as.numeric(x)[groups==levels(groups)[level] ], 
				plot=FALSE,
				breaks=hist.master$breaks,
				warn.unused=FALSE,
				...
			)
			hist.level$density <- hist.level$density * props[level]
			hist.level$rdensity <- hist.level$density / hist.master$density 
			switch( stripes, 
				vertical = 
				grid.rect(
					x=hist.level$breaks[-nbreaks] + cumrdensity*diff(breaks),
					y=0,
					width=diff(hist.level$breaks) * hist.level$rdensity,
					height=hist.master$height,
					just=c('left','bottom'),
					default.units='native',
					gp=gpar(col=fcol[level], fill=fcol[level],alpha=alpha),
					),
				horizontal = 
				grid.rect(
					x=hist.level$breaks[-nbreaks],
					y=0 + cumrdensity* hist.master$density,
					width=diff(hist.level$breaks),
					height=hist.master$height * hist.level$rdensity,
					just=c('left','bottom'),
					default.units='native',
					gp=gpar(col=fcol[level], fill=fcol[level],alpha=alpha),
					),
				none=
				grid.rect(
					x=hist.level$breaks[-nbreaks],
					y=0,
					width=diff(hist.level$breaks),
					height=hist.level$height,
					just=c('left','bottom'),
					default.units='native',
					gp=gpar(col='black', fill=fcol[level],alpha=alpha),
					)
			)
			cumdensity <- cumdensity + hist.level$density
			cumrdensity <- cumrdensity + hist.level$rdensity
			if (stripes != 'none') {
			grid.rect(
				x=hist.master$breaks[-nbreaks],
				y=0,
				width=diff(hist.master$breaks),
				height=hist.master$height,
				just=c('left','bottom'),
				default.units='native',
				gp=gpar(col='black', fill='transparent'),
				)
			}
		}
		if (verbose) { print(hist.master) }
	} else {
    	panel.histogram(x, type = type, breaks=breaks, ...)
	}
    if (labels) {
        myhist <- hist(x, plot = FALSE, warn.unused=FALSE, breaks=breaks, ...)
        if (type == "count") {
            aa <- max(myhist$counts) * 0.02
            grid.text(label = as.character(round(myhist$counts, 3)), 
                x = myhist$mids, y = aa + myhist$counts, just = c("centre", 
                  "bottom"), default.units = "native")
        }
        else if (type == "percent") {
            sumCounts <- sum(myhist$counts)
            aa <- max(myhist$counts/sumCounts) * 0.04
            grid.text(label = as.character(round(100*myhist$counts/sumCounts, 
                1)), x = myhist$mids, y = aa + (100*myhist$counts/sumCounts), just = c("centre", 
                "bottom"), default.units = "native")
        }
        else {
            aa <- max(myhist$density) * 0.02
            grid.text(label = as.character(round(myhist$density, 3)), 
                x = myhist$mids, y = aa + myhist$density, just = c("centre", 
                  "bottom"), default.units = "native")
        }
    }
    if (!is.null(fit)) {
        x = x[!is.na(x)]
        density <- TRUE
        if (is.null(args)) {
			if (! require(MASS) ){
				stop("The MASS package must be loaded to auto-fit distributions.")
			}
            if (is.null(start)) {
                args = fitdistr(x, fit)$estimate
            }
            else {
                args = fitdistr(x, fit, start = start)$estimate
            }
        }

		dmath = switch( tolower(fit), 
					   "exponential" = dexp,
					   "normal"      = dnorm,
					   "lognormal"   = dlnorm,
					   "log-normal"  = dlnorm,
       				   "poisson"     = dpois,
					   "beta"        = dbeta,
					   "t"           = dt,
					   "weibull"     = dweibull,
					   "cauchy"      = dcauchy,
					   "gamma"       = dgamma,
					   "chisq"       = dchisq,
					   "chi-squared" = dchisq
					   )
        } 

    if (is.null(args)) {
        args = list(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T))
    }
    if (density) {
		if (type != 'density') {
			warning("Use type='density' when adding density overlays.")
		}
		if (verbose) {
        	cat("args for density function:\n")
        	print(args)
		}
        panel.mathdensity(dmath = dmath, args = args, n = dn, 
            col = dcol, lwd = dlwd)
    }
    if (!missing(v)) {
        for (x in v) {
            panel.abline(v = x, col = gcol, lwd = glwd)
        }
    }
    if (!missing(h)) {
        for (y in h) {
            panel.abline(h = y, col = gcol, lwd = glwd)
        }
    }
}
