
#' Frequency Polygons
#' 
#' Frequency polygons are an alternative to histograms that make it simpler to overlay multiple
#' distributions.
#'
#' @param x a formula or a numeric vector
#' @param data a data frame in which to evaluate \code{x}
#' @param panel a panel function
#' @param type one of \code{'density'}, \code{'count'}, or \code{'percent'}
#' @param nint approximate number of bins
#' @param breaks break points for histogram bins, a function for computing such,
#'        or a method \code{\link{hist}} knows about given as a character string.
#'        If missing, \code{\link[mosaic]{xhistogramBreaks}} is used.
#'        
#' @param \dots additional arguments passed to \code{\link[lattice]{histogram}} and on to
#' 	\code{\link{panel.freqpolygon}}
#'
#' @return a trellis object
#' 
#' @export
#' @examples
#' freqpolygon(~age | substance, HELPrct, v=35, fit='normal')
#' freqpolygon(~age, HELPrct, labels=TRUE, type='count')
#' freqpolygon(~age, HELPrct, groups=cut(age, seq(10,80,by=10)))
#' freqpolygon(~age, HELPrct, groups=sex, stripes='horizontal')
#' freqpolygon(~racegrp, HELPrct, groups=substance,auto.key=TRUE)


freqpolygon <- function (x, data=parent.frame(), panel=panel.freqpolygon, type='density', 
						center=NULL, width=NULL, breaks, nint, ...) {
	if ( missing(breaks) ) {
		if (inherits(x,"formula")) {
			if (is.null(data)) {
				eval(rhs(x), parent.env())
			} else {
				xvals <- eval(rhs(x), data)
			}
		} else {
			xvals <- x
		}
		breaks <- xhistogramBreaks(xvals, center=center, width=width, nint=nint)
	}

	histogram(x, data=data, panel=panel, type=type, center=center, width=width, 
			  nint=substitute(nint), breaks=breaks, ...)
}


#' @rdname freqpolygon
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

panel.freqpolygon <-
function (x, 
	dcol = trellis.par.get("plot.line")$col, dlwd = 2, 
    col = trellis.par.get("add.line")$col, glwd = 2, 
	lwd = trellis.par.get("plot.line")$lwd,
	lty = trellis.par.get("plot.line")$lty,
	dmath = dnorm, 
	verbose = FALSE,
    dn = 100, args = NULL, labels = FALSE, density = FALSE, fit = NULL, 
    start = NULL, type = "density", v, h, groups=NULL, center=NULL, width=NULL, breaks,
    nint = round(1.5 * log2(length(x)) + 1),
	alpha=1, 
	...) 


{
	if (missing(breaks) || is.null(breaks)) {
		breaks <- xhistogramBreaks(x, center=center, width=width, nint=nint)
	} 
	if ( !is.null(groups) ) {
		panel.superpose(x, 
						groups = groups, 
						panel.groups = panel.freqpolygon, 
						type = type, center=center, width=width, breaks=breaks, 
						nint=nint, alpha=alpha, ...)
	} else {
		hist.master <- hist(as.numeric(x), plot = FALSE, breaks=breaks, warn.unused=FALSE, ...)
		hist.master$height <- switch(type,
									 'density' = hist.master$density,
									 'count' = hist.master$count,
									 'percent' = 100 * hist.master$count / length(x)
									 )
		nbreaks <- length(hist.master$breaks)
		panel.lines(
				   x=hist.master$breaks[-nbreaks] + .5 * diff(hist.master$breaks),
				   y=hist.master$height,
				   default.units='native',
				   col=col, alpha=alpha, lwd=lwd, lty=lty
				   )
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

panel.freqpolygon2 <- function (x, breaks, equal.widths = TRUE, 
							   type = "density", 
							   nint = round(log2(length(x)) + 1), 
							   alpha = plot.line$alpha, 
							   col = plot.line$col, 
    						   lty = plot.line$lty, 
							   lwd = plot.line$lwd, ..., 
							   identifier = "freqpolygon") 
{
	plot.line <- trellis.par.get("plot.line")
    xscale <- current.panel.limits()$xlim
    panel.lines(x = xscale[1] + diff(xscale) * c(0.05, 0.95), 
        y = c(0, 0), col = "black", lty = lty, lwd = lwd, alpha = alpha, 
        identifier = paste(identifier, "baseline", sep = "."))
    if (length(x) > 0) {
        if (is.null(breaks)) {
            breaks <- if (is.factor(x)) 
                seq_len(1 + nlevels(x)) - 0.5
            else if (equal.widths) 
                do.breaks(range(x, finite = TRUE), nint)
            else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <- lattice:::hist.constructor(x, breaks = breaks, ...)
        y <- if (type == "count") 
            h$counts
        else if (type == "percent") 
            100 * h$counts/length(x)
        else h$intensities
        breaks <- h$breaks
        nb <- length(breaks)
        if (length(y) != nb - 1) 
            warning("problem with 'hist' computations")
        if (nb > 1) {
			panel.lines(x = breaks[-nb] + 0.5 * diff(breaks), y = y, 
                col = col, alpha = alpha, lty = lty, 
                lwd = lwd, identifier = identifier)
        }
    }
}















