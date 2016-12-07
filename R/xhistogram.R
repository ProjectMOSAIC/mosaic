#' Augmented histograms
#' 
#' The \pkg{mosaic} package adds some additional functionality to 
#' \code{\link[lattice]{histogram}()}, making it simpler to obtain certain common 
#' histogram adornments.  This is done be resetting the default panel
#' and prepanel functions used by histogram.
#' 
#' @rdname xhistogram
#' @aliases histogram
#' @param x a formula or a numeric vector
#' @param panel a panel function
#' @param type one of \code{'density'}, \code{'count'}, or \code{'percent'}
#' @param nint approximate number of bins
#' @param breaks break points for histogram bins, a function for computing such,
#'        or a method \code{\link{hist}} knows about given as a character string.
#'        When using the \pkg{mosaic} package defaults, 
#'        \code{\link[mosaic]{xhistogramBreaks}} is used.
#' @param \dots additional arguments passed from \code{\link[lattice]{histogram}} 
#' to the panel function; by default when the \pkg{mosaic} package has been loaded this will
#' \code{\link{panel.xhistogram}}.
#'
#' @seealso \code{\link[lattice]{histogram}}, \code{\link{mosaicLatticeOptions}()},
#' and \code{\link{restoreLatticeOptions}()}.
#' 
#' @note Versions of \pkg{lattice} since 0.20-21 support setting custom defaults 
#' for \code{breaks}, \code{panel}, and \code{prepanel} used by 
#' \code{histogram()}, so \code{xhistogram()} is no longer needed.  
#' As a result, \code{xhistogram()} 
#' (which was required in earlier versions of \pkg{mosaic}
#' is no longer needed and has been removed.
#' 
#' @details
#' The primary additional functionality added to \code{\link{histogram}()}
#' are the arguments \code{width} and \code{center} which provide a simple
#' way of describing equal-sized bins, and \code{fit} which can be used to
#' overlay the density curve for one of several distributions.  The
#' \code{groups} argument can be used to color the bins.  The primary use
#' for this is to shade tails of histograms, but there may be other uses
#' as well.
#'
#' @export

xhistogram <- function (...) { # x, data=NULL, panel=panel.xhistogram, type='density', 
                               #       center=NULL, width=NULL, ...) {
   .Defunct(msg = "xhistogram() is no longer needed; simply use histogram() with the mosaic package attached.")
}


#' @rdname xhistogram
#' @return \code{xhistogramBreaks} returns a vector of break points
#' @examples
#' if (require(mosaicData)) {
#' histogram(~age | substance, HELPrct, v=35, fit='normal')
#' histogram(~age, HELPrct, labels=TRUE, type='count')
#' histogram(~age, HELPrct, groups=cut(age, seq(10,80,by=10)))
#' histogram(~age, HELPrct, groups=sex, stripes='horizontal')
#' histogram(~racegrp, HELPrct, groups=substance,auto.key=TRUE)
#' xhistogramBreaks(1:10, center=5, width=1)
#' xhistogramBreaks(1:10, center=5, width=2)
#' xhistogramBreaks(0:10, center=15, width=3)
#' xhistogramBreaks(1:100, center=50, width=3)
#' xhistogramBreaks(0:10, center=5, nint=5)
#' }
#' @export

xhistogramBreaks <- function(x, center=NULL, width=NULL, nint, ...) {
  x <- x[!is.na(x)]
  if (is.factor(x)) return(seq_len(1 + nlevels(x)) - 0.5)
  if (length(x) < 2) return (x)
  
  if (is.null(center)) { center <- 0 }
  if (missing(nint) || is.null(nint)) { 
    nint <- round(1.5 *log2(length(x)) + 1) 
  }
  
  if (is.null(width)) { 
    nint <- max(nint - 1, 1)
    width <- diff(range(x)) / nint
  }
  if (width <= 0) { width <- diff(range(c(0,x))) / nint }
  
  if (width <= 0) {stop("`width' too small.")}

  shift <- ( (floor( (min(x) - center)/width) ):(1 + ceiling( (max(x) - center)/width)) )
  breaks <-  -.5 * width + center + shift * width
# was an attempt to pretty-up the breaks
#  digits <- 15
#  while ( digits > 2 && diff(range(diff(breaks))) > 0) {
#    digits <- digits - 1 
#    width <- round(width,digits)
#    breaks <-  -.5 * width + center + shift * width
#  }
  
  if (breaks[2] < min(x)) breaks <- tail(breaks,-1)
  if (breaks[length(breaks)-1] > max(x)) breaks <- head(breaks,-1)
  
  if (min(breaks) > min(x) || max(breaks) < max(x)) 
	  stop("Bug alert: break points don't cover data.")
  return(breaks)
}

#' @rdname xhistogram
#' @export

prepanel.xhistogram <- 
  function (x, breaks=xhistogramBreaks, ...) 
  {
    if (is.function(breaks))  {
      breaks <- breaks(x, ...)
    }
    lattice::prepanel.default.histogram(x, breaks = breaks, ...)
  }

#' @rdname xhistogram
#' @param dcol color of density curve
#' @param dalpha alpha for density curve
#' @param gcol color of guidelines
#' @param fcol fill colors for histogram rectangles when using \code{groups}.  
#' (Use \code{col}, which is passed through to \code{\link{panel.histogram}}(), when 
#' not using `groups`.
#' 
#' @param dmath density function for density curve overlay
#' @param verbose be verbose?
#' @param dn number of points to sample from density curve
#' @param dlwd,glwd like \code{lwd} but affecting the density line and guide lines, respectively
#' @param args a list of additional arguments for \code{dmath}
#' @param labels should counts/densities/precents be displayed or each bin?
#' @param density a logical indicating whether to overlay a density curve
#' @param under a logical indicating whether the density layers should be under or 
#' over other layers of the plot.
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
#' @export
panel.xhistogram <-
function (x, 
	dcol = trellis.par.get("plot.line")$col, dalpha=1, dlwd = 2, 
  gcol = trellis.par.get("add.line")$col, glwd = 2, 
	fcol = trellis.par.get("superpose.polygon")$col,
	dmath = dnorm, 
	verbose = FALSE,
    dn = 100, args = NULL, labels = FALSE, density = NULL, under=FALSE, fit = NULL, 
    start = NULL, type = "density", v, h, groups=NULL, center=NULL, width=NULL, breaks,
    nint = round(1.5 * log2(length(x)) + 1),
	stripes=c('vertical','horizontal','none'), alpha=1, ...) 
{
  if (is.null(density)) density <- under
  if (missing(breaks) || is.null(breaks)) {
    breaks <- xhistogramBreaks(x, center=center, width=width, nint=nint)
  } 
  if (is.function(breaks))   {
    breaks <- breaks(x, center = center, width = width, nint = nint, ...)
  }
  stripes <- match.arg(stripes)
  
  if (!is.null(fit)) {
    dmath = switch( tolower(fit), 
                    "exponential" = dexp,
                    "normal"      = dnorm,
                    "lognormal"   = dlnorm,
                    "log-normal"  = dlnorm,
                    "poisson"     = dpois,
                    "beta"        = dbeta,
                    "t"           = dt3,
                    "weibull"     = dweibull,
                    "cauchy"      = dcauchy,
                    "gamma"       = dgamma,
                    "chisq"       = dchisq,
                    "chi-squared" = dchisq
    )
    x = x[!is.na(x)]
    density <- TRUE
    if (is.null(args)) {
      # we now depend on MASS, so we don't have to check for it
	  #	if (! require(MASS) ){
      #  stop("The MASS package must be loaded to auto-fit distributions.")
      #}

      if (is.null(start)) {
        args = fitdistr(x, fit)$estimate
      }
      else {
        args = fitdistr(x, fit, start = start)$estimate
      }
    }
  } 
  if (is.null(args)) {
    args = list(mean = base::mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE))
  }
  
  ###  done cleaining up args; away we go
  if (density && type != 'density') {
    warning("Use type='density' when adding density overlays.")
  }
  if (density && verbose) {
    cat("args for density function:\n")
    print(args)
  }
  if ( density && under ) {  # else do this near the end
    panel.mathdensity(dmath = dmath, args = args, n = dn, 
                      col = dcol, alpha=dalpha,lwd = dlwd)
  }

  ## plotting main part of histogram 
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
  ## additional adornments
  
  if ( density && !under ) {
    panel.mathdensity(dmath = dmath, args = args, n = dn, 
                      col = dcol, alpha=dalpha,lwd = dlwd)
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

dt3 <- function (x, df, m=0, s=1, log = FALSE) {
  if (log) 
    return( dt( (x-m)/s, df=df, log=TRUE)  - log(s) )
  dt( (x-m)/s, df=df, log=FALSE) / s
}  
