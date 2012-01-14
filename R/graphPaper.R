#' Traditional graph-paper axes 
#'
#' \code{graphPaper} plots out traditional graph-paper axes for making
#' mathematical plots.  Whereas standard R graphics put the axes at the
#' edge of the plotting window, \code{graphPaper} puts them through the zeros
#' and lets you set explicitly the location of tick marks and graph-paper rules.
#' You can then plot over the axes by using functions such as \code{lines},
#' \code{points}, or \code{funPlot} (with \code{add=TRUE}).
#'
#' @name graphPaper
#'
#' @param xticks numerical vector listing the position of x-axis ticks
#' @param yticks like \code{xticks} but for y-axis
#' @param xlim as in \code{plot}, sets x-axis limits (otherwise inferred from \code{xticks})
#' @param ylim similarly, set y-axis limits (otherwise inferred from \code{yticks})
#' @param xlab as in \code{plot}, sets the y-axis label
#' @param ylab similarly, set the y-axis label
#' @param maxxlabels limits the maximum number of tick marks on x-axis
#' @param maxylabels limits the maximum number of tick marks on y-axis
#' @param \dots additional arguments passed to plot
#'
#' @examples
#' graphPaper(xticks=-5:5, yticks=seq(-2, 2, by=.5))
#' plotFun(sin(x^2)~x, x=range(-4, 4), add=TRUE)
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @return nothing
graphPaper = function(xticks=0:5, yticks=-5:5,
					  xlim=c(min(xticks), max(xticks)),
					  ylim=c(min(yticks), max(yticks)),
					  xlab="x",
					  ylab="y", maxxlabels=7, maxylabels=7, ...) {
	axisYlabel <- ""
	axisXlabel <- ""
	if (nchar(ylab) > 3) axisYlabel <- ylab
	if (nchar(xlab) > 3) axisXlabel <- xlab

	plot( 0:1, type="n", xaxt="n", yaxt="n", bty="n", xlim=xlim, ylim=ylim,
		 xlab=axisXlabel, ylab=axisYlabel, ...)
	for (k in 1:length(xticks) )
		lines(xticks[k]*c(1, 1), ylim, col="gray", lwd=1)

	for (k in 1:length(yticks) )
		lines(xlim, yticks[k]*c(1, 1), col="gray", lwd=1)

	# Make sure there aren't too many labels on the axis

	if( length(xticks) > maxxlabels ) {
		skipfactor <- ceiling( length(xticks)/maxxlabels)
		xticks <- xticks[ c(1, -seq(-length(xticks), -2, by=skipfactor)) ]
	}

	if( length(yticks) > maxylabels ) {
		skipfactor <- ceiling( length(yticks)/maxylabels)
		yticks <- yticks[ c(1, -seq(-length(yticks), -2, by <- skipfactor)) ]
	}
	xlabels <- paste(xticks)
	ylabels <- paste(yticks)
	# see if zero in is the scale
	ypos <- min(ylim)
	if( 0 >= min(ylim) & 0 <= max(ylim) ) {
		ypos <- 0
		xlabels[xticks==0] <- "";
	}
	xpos <- min(xlim)
	if( 0 >= min(xlim) & 0 <= max(xlim) ) {
		xpos <- 0
		ylabels[yticks==0] <- "";
	}
	axis(1, at=xticks, labels=xlabels, pos=ypos, lwd=2, hadj=1)
	axis(2, at=yticks, labels=ylabels, pos=xpos, las=2, lwd=2)
	if ( axisXlabel=="") text( max(xlim), ypos, xlab, pos=3)
	if ( axisYlabel=="") text( xpos, max(ylim), ylab, pos=3, srt=90)

}



