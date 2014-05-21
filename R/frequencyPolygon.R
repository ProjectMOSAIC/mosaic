
#' Frequency Polygons
#' 
#' Frequency polygons are an alternative to histograms that make it simpler to overlay multiple
#' distributions.
#'
#' @param x a formula or a numeric vector
#' @param \dots additional arguments passed on to \code{\link{histogram}} 
#' and \code{panel}.
#' @param panel a panel function
#'
#' @return a trellis object
#' @note This function make use of \code{histogram} to determine overall layout.  Often 
#' this works reasonably well but sometimes it does not.  In the latter cases, it may be i
#' necessary to use \code{ylim} to determine an approprate viewing rectangle for the plot.
#' 
#' @export
#' @examples
#' freqpolygon(~age | substance, HELPrct, v=35, fit='normal')
#' freqpolygon(~age, HELPrct, labels=TRUE, type='count')
#' freqpolygon(~age, HELPrct, groups=cut(age, seq(10,80,by=10)))
#' freqpolygon(~age, HELPrct, groups=sex, stripes='horizontal')
#' freqpolygon(~racegrp, HELPrct, groups=substance,auto.key=TRUE)
#' ## comparison of histogram and frequency polygon
#' histogram(~eruptions, faithful, type='density', width=.5)
#' ladd( panel.freqpolygon(faithful$eruptions, width=.5 ))

freqpolygon <- function(x, 
                        ..., 
                        panel="panel.freqpolygon") {
  histogram(x, ..., panel=panel)
}



#' @rdname freqpolygon
#' @param plot.points one of \code{TRUE}, \code{FALSE}, \code{"jitter"}, or \code{"rug"} indicating
#' how points are to be displayed
#' @param gcol color of guidelines
#' @param glwd width of guidelines
#' @param groups,weights,jitter.amount,identifier as in \code{\link{densityplot}} 
#' or \code{\link{histogram}}
#' @param type one of \code{'density'}, \code{'percent'}, or \code{'count'}
#' @param breaks a vector of breaks for the frequency polygon bins
#' @param nint an approximate number of bins for the frequency polygon
#' @param center center of one of the bins
#' @param width width of the bins
#' @param wdth alternative to \code{width} to avoid conflict with \code{densityplot} argument
#' names 
#' @param h,v a vector of values for additional horizontal and vertical lines
#' @param ref a logical indicating whether a horizontal reference line should be 
#' added (roughly equivalent to \code{h=0})
#' @details These functions are still under development.  Future improvements may be forthcoming.
#' 
panel.freqpolygon <- function (x, plot.points = "jitter", ref = FALSE, 
          groups = NULL, weights = NULL, 
          jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
          type='density', 
          breaks=NULL, 
          nint= NULL,
          center=NULL, 
          wdth=NULL,
          width=wdth,
          gcol=trellis.par.get('reference.line')$col,
          glwd=trellis.par.get('reference.line')$lwd,
          h, v, 
          ..., identifier = "density") 
{
  if (missing(breaks) || is.null(breaks)) {
    breaks <- xhistogramBreaks(x, center=center, width=width, nint=nint)
  } 
  
	if (ref) {
		reference.line <- trellis.par.get("reference.line")
		panel.abline(h = 0, col = reference.line$col, lty = reference.line$lty, 
					 lwd = reference.line$lwd, identifier = paste(identifier, "abline"))
	}
	if (!is.null(groups)) {
		return(panel.superpose(x, plot.points = plot.points, 
						ref = FALSE, groups = groups, weights = weights, 
						panel.groups = panel.freqpolygon, jitter.amount = jitter.amount, 
						type = type, breaks=breaks, nint=nint, ...))
	}
	else {
		switch(as.character(plot.points), 
			   `TRUE` = panel.xyplot(x = x, y = rep(0, length(x)), type = type, ..., identifier = identifier), 
			   rug = panel.rug(x = x, start = 0, end = 0, 
							   x.units = c("npc", "native"), type = type, ..., 
							   identifier = paste(identifier, "rug")), 
			   jitter = panel.xyplot(x = x, y = jitter(rep(0, length(x)), amount = jitter.amount), 
									 type = 'p', ..., identifier = identifier))                                     

	}    
	hist.master <- hist(as.numeric(x), plot = FALSE, breaks=breaks, warn.unused=FALSE, ...)

	hist.master$height <- switch(type,
	                             'density' = hist.master$density,
	                             'count' = hist.master$count,
	                             'percent' = 100 * hist.master$count / length(x),
                               hist.master$density
	)

	nbreaks <- length(hist.master$breaks)
	hist.master$height <- c(0, hist.master$height, 0)
	hist.master$mids<- c(hist.master$mids[1] - diff(hist.master$mids[1:2]),  
	                     hist.master$mids,
	                     hist.master$mids[nbreaks-1] + diff(hist.master$mids[(nbreaks-2):(nbreaks-1)])
                       )
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
  panel.xyplot(x=hist.master$mids, 
	             y=hist.master$height,
	             default.units='native',
	             type='l', ...
	             )

}

