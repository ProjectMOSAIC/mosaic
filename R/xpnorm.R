tryCatch(utils::globalVariables( c('SD','Q','MEAN','slider','manipulate')),
		 error=function(e) message('Looks like you should update R.'))
#' Augmented versions of pnorm and qnorm
#' 
#' These functions behave similarly to the functions with the initial \code{x}
#' removed from their names but add more verbose output and graphics.
#'
#' @param p probability
#' @param q quantile
#' @param mean,sd parameters of normal distribution.
#' @param plot logical.  If TRUE, show an illustrative plot.
#' @param verbose logical.  If TRUE, display verbose output.
#' @param invisible logical.  If TRUE, return value invisibly.
#' @param digits number of digits to display in output.
#' @param lower.tail logical.  If FALSE, use upper tail probabilities.
#' @param log.p logical.  If TRUE, uses the log of probabilities.
#' @param xlim,ylim limits for plotting.
#' @param vlwd,vcol line width and color for vertical lines.
#' @param rot angle of rotation for text labels.
#' @param manipulate logical.  If TRUE and in RStudio,
#' 	then sliders are added for ineractivity.
#' @param \dots additional arguments.
#' 
#' 
#' @seealso \code{\link{histogram}}, 
#' \code{\link{chisq.test}}, 
#' \code{\link{pnorm}}, 
#' \code{\link{qnorm}}, 
#' \code{\link{qqmath}}, and
#' \code{\link{plot}}. 
#' 
#' 
#' @export
#' @examples
#' xpnorm(650, 500, 100)
#' xqnorm(.75, 500, 100)
#' \dontrun{
#' if (require(manipulate)) {
#'   manipulate( xpnorm(score, 500, 100, verbose=verbose),
#'     score = slider(200,800),
#' 	   verbose = checkbox(TRUE, label="Verbose Output")
#'   )
#' }
#' }

xpnorm <-
function (q, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, invisible=FALSE, digits = 4, 
    lower.tail = TRUE, log.p = FALSE, xlim = mean + c(-4,4) * sd, ylim = c(0, 1.4 * dnorm(mean,mean,sd)), 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	rot=45, manipulate=FALSE, ...) 
{
	if ( manipulate && require(manipulate) ) {
		return(manipulate( 
			xpnorm(q=Q, mean=MEAN, sd=SD, plot=TRUE, verbose=FALSE, invisible=invisible,
						   digits=digits, lower.tail=lower.tail, log.p=log.p, xlim=xlim,
						   ylim=ylim, vlwd=vlwd, vcol=vcol, rot=rot, manipulate=FALSE,
						   ...),
				   Q = slider(mean-4*sd, mean+4*sd, initial=q, step=8*sd/200, label='q'),
				   MEAN = slider(mean-4*sd, mean+4*sd, initial=mean, step=8*sd/200, label='mean'),
				   SD = slider(0, 4*sd, initial=sd, step=4*sd/100, label='st dev')
				   )
		)
	}
    p = pnorm(q, mean = mean, sd = sd) 
    z = (q - mean)/sd
    if (verbose) {
		cat("\n")
		cat(paste("If X ~ N(",mean,",",sd,"), then \n\n",sep=""))
        cat(paste("\tP(X <= ", q, ") = P(Z <= ", round(z, 3), 
            ") = ", round(p,digits), "\n", sep = ""))
        cat(paste("\tP(X >  ", q, ") = P(Z >  ", round(z, 3), 
            ") = ", round(1 - p,digits), "\n", sep = ""))
        cat("\n")
    }
    if (plot & length(q) == 1) {
		print(.plot_one_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      vlwd=vlwd, vcol=vcol, rot=rot, ...))
    }
    if (plot & length(q) > 1) {
			print(.plot_multi_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      vlwd=vlwd, vcol=vcol, rot=rot, ...))
    }
	if (invisible) { 
    	return(invisible(pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p)))
	}
    return(pnorm(q, mean = mean, sd = sd, lower.tail = lower.tail, log.p = log.p))
}

#' @rdname xpnorm

xqnorm <-
function (p, mean = 0, sd = 1, plot = TRUE, verbose = TRUE, digits = 4, 
    lower.tail = TRUE, log.p = FALSE, xlim, ylim, invisible=FALSE, 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	rot=45, ...) 
{
    q = qnorm(p, mean = mean, sd = sd, lower.tail = lower.tail, 
        log.p = log.p)
    z = (q - mean)/sd
    if (verbose) {
        cat(paste("\tP(X <= ", q, ") = ", p, "\n", sep = ""))
        cat(paste("\tP(X >  ", q, ") = ", 1 - p, "\n", sep = ""))
        cat("\n")
    }
    if (plot & length(p) == 1) {
		print(.plot_one_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      vlwd=vlwd, vcol=vcol, rot=rot, ...))
    }
    if (plot & length(p) > 1) {
		print(.plot_multi_norm(p=p, q=q, mean, sd, xlim=xlim, ylim=ylim, digits=digits, 
		      vlwd=vlwd, vcol=vcol, rot=rot, ...))
    }
	if (invisible) { 
    		return(invisible(q))
	}
    return(q)
}

# midpoints along a sequence
.mid <- function(x) { 
	x[-1] - .5 * diff(x)
}


.plot_multi_norm <- function(p, q, mean, sd, xlim, ylim, digits=4, dlwd=2, 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	rot=0, ...) 
{
	dots <- list(...)

	if (! 'lty' %in% names(dots)) { dots$lty <- 1 }

	z <- (q - mean) / sd
			z <- (q - mean) / sd 
	zmax = max(4, abs(z) * 1.6)
	if (missing(xlim)) {
		xlim = mean + c(-1, 1) * abs(zmax) * sd
	}
	ymax = dnorm(mean, mean = mean, sd = sd)
	if (missing(ylim)) {
		ylim = c(0, 1.4 * ymax)
	}
	xdata = seq(xlim[1], xlim[2], length.out=400)
	ydata = dnorm(xdata, mean=mean, sd=sd)
	groups = apply(sapply(xdata, function(x) {x < q}), 2, sum)

	p <- c(0, p, 1)
	q <- c(xlim[1], q, xlim[2])

	plot <- do.call("xyplot", c(list(
		ydata ~ xdata, 
		xlim = xlim, ylim = ylim, 
		groups = groups, type='h',
		xlab = "", ylab = "density", 
		panel = function(x, y, ...) {
			panel.xyplot(x,y,...)
			panel.segments(q, 0, q, unit(ymax,'native') + unit(.2,'lines'), 
			  col = vcol, lwd=vlwd)
			grid.text(x=.mid(q), y=unit(ymax,'native') + unit(1.0,'lines'), default.units='native',
				rot=rot,
				check.overlap=TRUE,
			  	paste("", round(diff(p), 3), sep = ""), 
				just = c('center','center'),  gp=gpar(cex = 1.0))
			panel.mathdensity(dmath = dnorm, args = list(mean = mean, 
			  sd = sd), lwd = dlwd, n = 100, col = "navy")
		} 
		), dots)
	)

	return(plot)
}


.plot_one_norm <- function(p, q, mean, sd, xlim, ylim, digits=4, 
    vlwd=2, vcol=trellis.par.get('add.line')$col,
	...) 
{
	z <- (q - mean) / sd
			z <- (q - mean) / sd 
	zmax = max(4, abs(z) * 1.6)
	if (missing(xlim)) {
		xlim = mean + c(-1, 1) * zmax * sd
	}
	ymax = dnorm(mean, mean = mean, sd = sd)
	if (missing(ylim)) {
		ylim = c(0, 1.4 * ymax)
	}
	xdata = rep(xlim, each = 2)
	ydata = rep(ylim, times = 2)

	plot <- xyplot(ydata ~ xdata, xlim = xlim, ylim = ylim, 
		xlab = "", ylab = "density", 
		panel = function(x, y, ...) {
			panel.mathdensity(dmath = dnorm, args = list(mean = mean, 
			  sd = sd), lwd = 2, n = 100, col = "navy")
			xs <- seq(xlim[1], q, by=diff(xlim)/500)
			panel.xyplot(xs, dnorm(xs, mean, sd) , type='h')
			if (dnorm(q, mean, sd) > 0.5 * ymax) {
			  textloc = c(q, ymax * 0.2)
			  textloc = c(q, 1.2 * ymax)
			}
			else {
			  textloc = c(q, ymax * 0.8)
			  textloc = c(q, 1.2 * ymax)
			}
			panel.segments(q, 0, q, unit(ymax,'native') + unit(1.5,'lines'), 
			  col = vcol, lwd=vlwd)
			#panel.segments(q, textloc[2] + 0.1 * ymax, q, 
			#  ylim[2], col = "forestgreen")
			grid.text(x=q, y=unit(ymax,'native') + unit(2.4,'lines'),  default.units='native',
				paste(round(q, digits)), 
				just = c('center','bottom'), gp=gpar(cex = 1.5))
			grid.text(x=q, y=unit(ymax,'native') + unit(2.4,'lines'), default.units='native',
			  paste("(z=", round(z, 3), ")", sep = ""), 
				just = c('center','top'),  gp=gpar(cex = 1.2))
			grid.lines( gp=gpar(lwd=1.5),
				x=unit.c( unit(q,'native'), unit(q,'native') - unit(2,'char') ),
				y=unit(ymax,'native') + unit(.6,'lines'),
				arrow=arrow(angle=20,length=unit(.75,'char'))
				)
			grid.text(
				x=unit(q,'native') - unit(2,'char'), 
				y=unit(ymax,'native') + unit(.3,'lines'), default.units='native',
				paste(round(p, digits), ""), 
				just = c('right','bottom'),  gp=gpar(cex = 1.2))
			grid.lines( gp=gpar(lwd=1.5),
				x=unit.c( unit(q,'native'), unit(q,'native') + unit(2,'char') ),
				y=unit(ymax,'native') + unit(.6,'lines'),
				arrow=arrow(angle=20,length=unit(.75,'char'))
				)
			grid.text(
				x=unit(q,'native') + unit(2,'char'), 
				y=unit(ymax,'native') + unit(.3,'lines'), default.units='native',
				paste("", round(1 - p, digits)), 
				just = c('left','bottom'),  gp=gpar(cex = 1.2))
		}, ...)

	return(plot)
}


