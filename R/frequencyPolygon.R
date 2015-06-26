
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
#' this works reasonably well but sometimes it does not. In particular, when \code{groups} is
#' used to overlay multiple frequency polygons, there is often too little head room.  
#' In the latter cases, it may be 
#' necessary to use \code{ylim} to determine an approprate viewing rectangle for the 
#' plot.
#' 
#' @examples
#' if (require(mosaicData)) {
#' freqpolygon(~age | substance, data=HELPrct, v=35)
#' freqpolygon(~age, data=HELPrct, labels=TRUE, type='count')
#' freqpolygon(~age | substance, data=HELPrct, groups=sex)
#' freqpolygon(~age | substance, data=HELPrct, groups=sex, ylim=c(0,0.11))
#' ## comparison of histogram and frequency polygon
#' histogram(~eruptions, faithful, type='density', width=.5)
#' ladd( panel.freqpolygon(faithful$eruptions, width=.5 ))
#' }
#' @export

freqpolygon <- function(x, 
                        ..., 
                        panel="panel.freqpolygon",
                        prepanel="prepanel.default.freqpolygon"
                        ) {
  histogram(x, ..., panel=panel, prepanel = prepanel)
}

#' @rdname freqpolygon
#' @param darg a list of arguments for the function computing the frequency polygon.
#' @param groups as in other lattice plots
#' @param subscripts as in other lattice prepanel functionsa
#' @export
#' 
prepanel.default.freqpolygon <- function(
  x, plot.points = FALSE, ref = FALSE,
  groups = NULL, subscripts = TRUE, 
  jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
  center = NULL, nint = NULL, breaks=NULL, width = NULL, type = "density",
  ...) 
{
  if (!is.numeric(x)) 
    x <- as.numeric(x)
  if (missing(breaks) || is.null(breaks)) {
    breaks <- xhistogramBreaks(x, center=center, width=width, nint=nint)
  } 
  if (is.function(breaks) || is.character(breaks)) {
    breaks <- do.call(breaks, list(x=x, center=center, width=width, nint=nint, ...) )
  }
  
  if (sum(!is.na(x)) < 1) 
    prepanel.null()
  else if (sum(!is.na(x)) == 1) {
    list(xlim = rep(x, 2), ylim = rep(0, 2), dx = 1, dy = 1)
  }
  else if (is.null(groups)) {
    h <- hist(x, plot = FALSE, breaks=breaks, warn.unused=FALSE, ...)
    h$height <- 
      switch(
        type,
        'density' = h$density,
        'count' = h$count,
        'percent' = 100 * h$count / length(x),
        h$density
      )
    quants <- quantile(x, c(0.15, 0.85), names = FALSE, na.rm = TRUE)
    ok <- h$mids > quants[1] & h$mids < quants[2]
    print (breaks)
    list(xlim = range(h$mids), ylim = range(h$height), dx = diff(h$mids[ok]), 
         dy = diff(h$height[ok]))
  } else {
    vals <- sort(unique(groups))
    xl <- range(x, finite = TRUE)
    yl <- 0
    dxl <- numeric(0)
    dyl <- numeric(0)
    for (i in seq_along(vals)) {
      id <- (groups[subscripts] == vals[i])
      if (sum(id, na.rm = TRUE) > 1) {

        h <- do.call(hist, c(list(x = x[id], plot = FALSE, breaks = breaks, warn.unused = FALSE))) 
        h$height <- 
          switch(
            type,
            'density' = h$density,
            'count' = h$count,
            'percent' = 100 * h$count / length(x),
            h$density
          )
        xl <- c(xl, h$mids)
        yl <- c(yl, h$height)
        quants <- quantile(x[id], c(0.15, 0.85), names = FALSE, na.rm = TRUE)
        ok <- h$mids > quants[1] & h$mids < quants[2]
        dxl <- c(dxl, diff(h$mids[ok]))
        dyl <- c(dyl, diff(h$height[ok]))
      }
    }
    print(range(yl))
    list(xlim = range(xl, finite = TRUE), 
         ylim = range(yl, finite = TRUE), dx = dxl, dy = dyl)
  }
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
#' @export

panel.freqpolygon <- 
  function (
    x, 
    plot.points = FALSE, ref = FALSE, 
    groups = NULL, weights = NULL, 
    jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
    type='density', 
    breaks=NULL, 
    nint= NULL,
    center=NULL, 
#    wdth=NULL,
    width=NULL,
    gcol=trellis.par.get('reference.line')$col,
    glwd=trellis.par.get('reference.line')$lwd,
    h, v, 
    ..., identifier = "freqpoly") 
  {
    print(data.frame(where = "panel", 
                     w=if(is.null(width)) "NULL" else width, 
                     c=if(is.null(center)) "NULL" else center, 
                     nint= if(is.null(nint)) "NULL" else nint)
    )
    if (missing(breaks) || is.null(breaks)) {
      breaks <- xhistogramBreaks(x, center=center, width=width, nint=nint)
    } 
    if (is.function(breaks) || is.character(breaks)) {
      breaks <- do.call(breaks, list(x=x, center=center, width=width, nint=nint, ...) )
    }
    print(breaks)
    
	if (ref) {
		reference.line <- trellis.par.get("reference.line")
		panel.abline(h = 0, col = reference.line$col, lty = reference.line$lty, 
					 lwd = reference.line$lwd, identifier = paste(identifier, "abline"))
	}
	if (!is.null(groups)) {
		return(panel.superpose(x, plot.points = plot.points, 
						ref = FALSE, groups = groups, 
						panel.groups = panel.freqpolygon, jitter.amount = jitter.amount, 
						type = type, breaks=breaks, width = width, nint=nint, ...))
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

