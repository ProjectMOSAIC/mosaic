
#' Frequency Polygons
#' 
#' Frequency polygons are an alternative to histograms that make it simpler to overlay multiple
#' distributions.
#'
#' @param x a formula or a numeric vector
#' @param \dots additional arguments passed on to [histogram()] 
#' and `panel`.
#' @param panel a panel function
#' @param prepanel a prepanel function
#'
#' @return a trellis object
#' @note This function make use of `histogram` to determine overall layout.  Often 
#' this works reasonably well but sometimes it does not. In particular, when `groups` is
#' used to overlay multiple frequency polygons, there is often too little head room.  
#' In the latter cases, it may be 
#' necessary to use `ylim` to determine an appropriate viewing rectangle for the 
#' plot.
#' 
#' @examples
#' freqpolygon(~age | substance, data=HELPrct, v=35)
#' freqpolygon(~age, data=HELPrct, labels=TRUE, type='count')
#' freqpolygon(~age | substance, data=HELPrct, groups=sex)
#' freqpolygon(~age | substance, data=HELPrct, groups=sex, ylim=c(0,0.11))
#' ## comparison of histogram and frequency polygon
#' histogram(~eruptions, faithful, type='density', width=.5)
#' ladd( panel.freqpolygon(faithful$eruptions, width=.5 ))
#' @export

freqpolygon <- function(x, 
                        ..., 
                        panel="panel.freqpolygon",
                        prepanel="prepanel.default.freqpolygon"
) {
  densityplot(x, ..., panel=panel, prepanel = prepanel)
}

#' @rdname freqpolygon

#' @export
#' 
prepanel.default.freqpolygon <- function(
  x, darg = list(), plot.points = FALSE, ref = FALSE,
  groups = NULL, subscripts = TRUE, 
  jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
  center = NULL, nint = NULL, breaks=NULL, width = darg$width, type = "density",
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
    list(xlim = range(xl, finite = TRUE), 
         ylim = range(yl, finite = TRUE), dx = dxl, dy = dyl)
  }
}

#' Turn histograms into frequency polygons
#' 
#' Turn histograms into frequency polygons
#' 
#' @rdname freqpoly
#' @param x a vector of values for which a frequency polygon is desired.
#' @param plot a logical indicating if a plot should be generated.
#' @param ... additional arguments passed on to [hist()].
#' @return An object of class `"freqpoly"` (invisibly).  Additionally, if `plot` is 
#' `TRUE`, a plot is generated.
#' @export
#' @examples
#' freqpoly(faithful$eruptions)
#' bks <- c(0, 1, 1.5, 2, 3, 3.5, 4, 4.5, 5, 7)
#' hist(faithful$eruptions, breaks = bks)
#' freqpoly(faithful$eruptions, col = rgb(0,0,1,.5), lwd = 5, breaks = bks, add = TRUE)
#' 
freqpoly <- function(x, plot = TRUE, ...) {
  h <- hist(x, plot = FALSE, warn.unused = FALSE, ...)
  res <- hist2freqpolygon(h)
  res$xname = rlang::expr_text(x)
  dots <- list(...)
  # remove from ... things that plot() might not like.
  bad_args <- c("breaks", "include.lowest", "right", "density", "angle", 
                 "border", "warn.unused", "nclass")
  pm <- pmatch(names(dots), bad_args)
  dots[!is.na(pm)] <- NULL
  
  if (plot) do.call("plot", c(list(res), dots))
  invisible(res)
}

#' @export
#' @rdname freqpoly
#' @param hist a histogram object produced by \code{link{hist}()}.
#' 
hist2freqpolygon <- function(hist) {
  res <- hist
  if (hist$counts[1] > 0) {
    res$counts <- c(0, res$counts)
    res$density <- c(0, res$density)
    res$mids <- c(res$mids[1] - diff(res$mids[1:2]), res$mids)
    res$breaks <- c(res$breaks[1] - 2 * (res$breaks[1] - res$mids[1]), res$breaks)
  }
  
  if (tail(hist$counts, 1) > 0) {
    res$counts <- c(res$counts, 0)
    res$density <- c(res$density, 0)
    nbreaks <- length(res$breaks)
    res$mids <- c(res$mids, res$mids[nbreaks-1] + diff(res$mids[(nbreaks-2):(nbreaks-1)]))
    res$breaks <- c(res$breaks, res$breaks[nbreaks] + 2 * (res$mids[nbreaks] - res$breaks[nbreaks]))
  }
  
  width <- diff(res$breaks)  # one for each mid
  ratio <- tail(width, -1) / (tail(width, -1) + head(width, -1))
  brk_density <- head(res$density, -1) + ratio * diff(res$density)
  brk_counts <- head(res$counts, -1) + ratio * diff(res$counts)
  
  res <-
    dplyr::tibble(
      x = c(res$mids, head(tail(res$breaks, -1), -1)), 
      density = c(res$density, brk_density),
      counts = c(res$counts, brk_counts)) %>% 
    arrange(x) %>% 
    as.list()
  res$equidist <- hist$equidist
  res$xname <- hist$xname
  class(res) <- "freqpolygon"
  res
}

#' @rdname freqpoly
#' @param freq A logical indicating whether the vertical scale should be frequency (count).
#' @param col A color for the frequency polygon.
#' @param lty An integer indicating the line type.
#' @param lwd An integer indicating the line width.
#' @param main A title for the plot.
#' @param sub A sub-title for the plot.
#' @param xlab Label for the horizontal axis.
#' @param ylab Label for the vertical axis.
#' @param xlim A numeric vector of length 2.
#' @param ylim A numeric vector of length 2.
#' @param axes A logical indicating whether axes should be drawn.
#' @param labels A logical indicating whether labels should be printed or a character
#' vector of labels to add.
#' @param add A logical indicating whether the plot should be added to the current plot
#' @param ann A logical indicating whether annotations (titles and axis titles) should be plotted.
#' @export
plot.freqpolygon <- 
  function (x, freq = equidist, col = graphics::par("fg"), lty = NULL, lwd = 1, 
            main = paste("Frequency polygon of",  paste(x$xname, collapse = "\n")), 
            sub = NULL, xlab = x$xname, ylab, xlim = range(x$x), 
            ylim = NULL, axes = TRUE, labels = FALSE, 
            add = FALSE, ann = TRUE, ...) 
  {
    equidist <- if (is.logical(x$equidist)) 
      x$equidist
    else {
      h <- diff(x$breaks)
      diff(range(h)) < 1e-07 * mean(h)
    }
    
    if (freq && !equidist) 
      warning("the AREAS in the plot are wrong -- rather use 'freq = FALSE'")
    y <- if (freq) x$counts else x$density
    nB <- length(x$x)
    if (is.null(y) || 0L == nB) 
      stop("'x' is wrongly structured")
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush())
    if (!add) {
      if (is.null(ylim)) 
        ylim <- range(y, 0)
      if (missing(ylab)) 
        ylab <- if (!freq) 
          "Density"
      else "Frequency"
      graphics::plot.new()
      graphics::plot.window(xlim, ylim, "", ...)
      if (ann) 
        graphics::title(main = main, sub = sub, xlab = xlab, ylab = ylab, 
              ...)
      if (axes) {
        graphics::axis(1, ...)
        graphics::axis(2, ...)
      }
    }
    # rect(x$breaks[-nB], 0, x$breaks[-1L], y, col = col, border = border, 
    #      angle = angle, density = density, lty = lty)
    graphics::lines(x$x, y, col = col, lty = lty, lwd = lwd) 
    if ((logl <- is.logical(labels) && labels) || is.character(labels)) 
      graphics::text(x$x[c(TRUE, FALSE)], y[c(TRUE, FALSE)], labels = if (logl) {
        if (freq) 
          x$counts[c(TRUE, FALSE)]
        else round(x$density[c(TRUE, FALSE)], 3)
      }
      else labels, adj = c(0.5, -0.5))
    invisible()
  }

#' @rdname freqpolygon
#' @param plot.points one of `TRUE`, `FALSE`, `"jitter"`, or `"rug"` indicating
#' how points are to be displayed
#' @param gcol color of guidelines
#' @param glwd width of guidelines
#' @param groups,weights,jitter.amount,identifier as in [densityplot()] 
#' or [histogram()]
#' @param type one of `'density'`, `'percent'`, or `'count'`
#' @param breaks a vector of breaks for the frequency polygon bins
#' @param nint an approximate number of bins for the frequency polygon
#' @param center center of one of the bins
#' @param width width of the bins
#' @param h,v a vector of values for additional horizontal and vertical lines
#' @param ref a logical indicating whether a horizontal reference line should be 
#' added (roughly equivalent to `h=0`)
#' @param darg a list of arguments for the function computing the frequency polygon.
#'   This exists primarily for compatibility with `densityplot` and is unlikely
#'   to be needed by the end user.
#' @param subscripts as in other lattice prepanel functions
#' @export

panel.freqpolygon <- 
  function (
    x, darg=list(),
    plot.points = FALSE, ref = FALSE, 
    groups = NULL, weights = NULL, 
    jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
    type='density', 
    breaks=NULL, 
    nint= NULL,
    center=NULL, 
    width = darg$width,
    gcol=trellis.par.get('reference.line')$col,
    glwd=trellis.par.get('reference.line')$lwd,
    h, v, 
    ..., identifier = "freqpoly") 
  {
    if (missing(breaks) || is.null(breaks)) {
      breaks <- xhistogramBreaks(x, center=center, width=width, nint=nint)
    } 
    if (is.function(breaks) || is.character(breaks)) {
      breaks <- do.call(breaks, list(x=x, center=center, width=width, nint=nint, ...) )
    }
    
    if (ref) {
      reference.line <- trellis.par.get("reference.line")
      panel.abline(h = 0, col = reference.line$col, lty = reference.line$lty, 
                   lwd = reference.line$lwd, identifier = paste(identifier, "abline"))
    }
    if (!is.null(groups)) {
      return(panel.superpose(x, darg = darg, plot.points = plot.points, 
                             ref = FALSE, groups = groups, 
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
    hist.master <- 
      hist2freqpolygon(hist(as.numeric(x), plot = FALSE, breaks=breaks, warn.unused=FALSE, ...))
    
    hist.master$height <- switch(type,
                                 'density' = hist.master$density,
                                 'count' = hist.master$counts,
                                 'percent' = 100 * hist.master$counts / length(x),
                                 hist.master$density
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
    panel.xyplot(x=hist.master$x, 
                 y=hist.master$height,
                 default.units='native',
                 type='l', ...
    )
    
  }

# copied from lattice because it isn't exported there.
prepanel.null <- function () 
{
  list(xlim = rep(NA_real_, 2), ylim = rep(NA_real_, 2), dx = NA_real_, 
       dy = NA_real_)
}
