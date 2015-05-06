
dpqrdist <- function( dist, type=c("d","p","q","r"), ... ) {
  type <- match.arg(type)
  dots <- list(...)
  distFunName <- paste0(type,dist)

  do.call(distFunName, dots)
}

#' Illustrated probability calculations from distributions
#' 
#' Illustrated probability calculations from distributions
#' 
#' @param dist a character discription of a distribution, for example 
#'   \code{"norm"}, \code{"t"}, or \code{"chisq"}
#' @param q a vector of quantiles
#' @param plot a logical indicating whether a plot should be created
#' @param verbose a logical
#' @param invisible a logical
#' @param digits the number of digits desired
#' @param xlim x limits
#' @param ylim y limits
#' @param vlwd width of vertical lines
#' @param vcol color of vertical lines
#' @param rot angle for rotating text indicating probability
#' @param ... additional arguments, including parameters of the distribution
#' and additional options for the plot
#' @details The most general function is \code{pdist} which can work with 
#' any distribution for which a p-function exists.  As a convenience, wrappers are 
#' provided for several common distributions.
#' @return a vector of probabilities; a plot is printed as a side effect 
#' @examples
#' pdist("norm", -2:2)
#' pdist("norm", seq(80,120, by=10), mean=100, sd=10)
#' pdist("chisq", 2:4, df=3)
#' pdist("f", 1, df1=2, df2=10)
#' pdist("gamma", 2, shape=3, rate=4)
#' @export
 
pdist <- function (dist="norm", q, plot = TRUE, verbose = FALSE, invisible=FALSE, 
                   digits = 4, 
                   xlim, ylim,
                   vlwd=2, 
                   vcol=trellis.par.get('add.line')$col, 
                   rot=45, 
                   resolution = 5000,
                   ...)
{
  
  dots <- list(...)
  
  p <- dpqrdist(dist, type="p", q, ...) 
  
  if (verbose) {
    cat("Verbose output not yet implemented.\n")
  }
  
  if (plot) {
    print(plot_multi_dist(dist=dist, p=p, 
                          xlim=xlim, ylim=ylim, 
                          digits=digits, 
                          resolution=resolution,
                          vlwd=vlwd, vcol=vcol, rot=rot, ...))
  }
  if (invisible) { 
    return(invisible(p))
  }
  return(p)
}

#' Illustrated quantile calculations from distributions
#' 
#' Illustrated quantile calculations from distributions
#' 
#' @param dist a character discription of a distribution, for example 
#'   \code{"norm"}, \code{"t"}, or \code{"chisq"}
#' @param p a vector of probabilities
#' @param plot a logical indicating whether a plot should be created
#' @param verbose a logical
#' @param invisible a logical
#' @param digits the number of digits desired
#' @param xlim x limits
#' @param ylim y limits
#' @param vlwd width of vertical lines
#' @param vcol color of vertical lines
#' @param rot angle for rotating text indicating probability
#' @param ... additional arguments, including parameters of the distribution
#' and additional options for the plot
#' @details The most general function is \code{qdist} which can work with 
#' any distribution for which a q-function exists.  As a convenience, wrappers are 
#' provided for several common distributions.
#' @return a vector of quantiles; a plot is printed as a side effect
#' @examples
#' qdist("norm", seq(.2, .8, by=.10))
#' xqnorm(seq(.2, .8, by=.10), mean=100, sd=10)
#' qdist("unif", .5)
#' xqgamma(.5, shape=3, scale=4)
#' xqchisq(c(.25,.5,.75), df=3)
#' @export 

qdist <- function (dist="norm", p, plot = TRUE, verbose = FALSE, invisible=FALSE, 
                   resolution = 5000,
                   digits = 4, 
                   xlim, ylim,
                   vlwd=2, 
                   vcol=trellis.par.get('add.line')$col, 
                   rot=45, 
                   ...)
{
  
  dots <- list(...)
  
  q <- dpqrdist(dist, type="q", p, ...) 
  if (verbose) {
    cat("Verbose output not yet implemented.\n")
  }
  
  if (plot) {
    print(plot_multi_dist(dist=dist, p=p, 
                          xlim=xlim, ylim=ylim, 
                          digits=digits, 
                          resolution=resolution,
                          vlwd=vlwd, vcol=vcol, rot=rot, ...))
  }
  if (invisible) { 
    return(invisible(p))
  }
  return(q)
}


plot_multi_dist <- function(dist, p, q, xlim, ylim, digits=4, resolution=5000,
                            dlwd=2, vlwd=2, vcol=trellis.par.get('add.line')$col,
                            rot=0, ...) 
{
  dots <- list(...)
  discrete <- is_discrete_dist(dist, ...)
  
  if (missing(p)) {
    if (missing(q)) { stop( "one of p or q must be specified") }
    q <- sort(q)
    p <- dpqrdist(dist, type="p", q=q, ...)
  } else {
    if (!missing(q)) {
      warning("Both p and q were specified.  I'm using p")
    }
    p <- sort(p)
    q <- dpqrdist(dist, type="q", p=p, ...)
  }
  
  if (! 'lty' %in% names(dots)) { dots$lty <- 1 }
  
  if (missing(xlim) || is.null(xlim)) {
    xlim <- dpqrdist(dist, type="q", p=c(0.001, .999), ...)
  }
  if (discrete) {
    xdata <- unique(dpqrdist(dist, type="q", ppoints(resolution), ...))
    step = min(diff(xdata))
    fill <- seq( min(xdata) -1.5 * step , max(xdata) + 1.5*step, length.out=resolution)
    xdata <- c(xdata, fill) 
    xdata <- dpqrdist(dist, type="q", dpqrdist(dist, type="p", xdata, ...), ...)
    xdata <- sort(unique(xdata))
  } else {
    xdata <- unique(seq(xlim[1], xlim[2], length.out=resolution))
  }
  
  ydata <- dpqrdist(dist, type="d", xdata, ...)
  ymax <- max(ydata, na.rm=TRUE)
  if (missing(ylim)) {
    ylim = c(0, 1.4 * ymax)
  }
  
  groups <- sapply(xdata, function(x) {sum(x < q)})
  
  # this could be funny if limits don't span q
  p <- c(0, p, 1)
  q <- c(xlim[1], q, xlim[2])
  
  latticedots <- dots[ ! names(dots) %in% names(formals(paste0("p",dist))) ]
  
  args <- c(
    list(
      ydata ~ xdata, 
      xlim = xlim, ylim = ylim, 
      groups = sapply(xdata, function(x) {sum(x < q)}),
      type='h',
      xlab = "", ylab = "density", 
      panel = 
        if (discrete) {
          function(x, y, ...) {
            panel.xyplot(x,y,...)
            panel.segments(q, 0, q, unit(ymax,'native') + unit(.2,'lines'), 
                           col = vcol, lwd=vlwd)
            grid.text(x=mid(q), y=unit(ymax,'native') + unit(1.0,'lines'), default.units='native',
                      rot=rot,
                      check.overlap=TRUE,
                      paste("", round(diff(p), 3), sep = ""), 
                      just = c('center','center'),  gp=gpar(cex = 1.0))
            panel.xyplot(x,y, type=c('p','h'), lwd=dlwd)
          } 
        } else {
          function(x, y, ...) {
            panel.xyplot(x, y, ...)
            panel.segments(q, 0, q, unit(ymax,'native') + unit(.2,'lines'), 
                           col = vcol, lwd=vlwd)
            grid.text(x=mid(q), y=unit(ymax,'native') + unit(1.0,'lines'), default.units='native',
                      rot=rot,
                      check.overlap=TRUE,
                      paste("", round(diff(p), 3), sep = ""), 
                      just = c('center','center'),  gp=gpar(cex = 1.0))
            panel.xyplot(x,y, type='l', lwd=dlwd, col="black")
          }
        } 
    ), 
    latticedots
  )
  
  plot <- do.call("xyplot", args)
  
  return(plot)
}


#' @rdname pdist
#' @seealso \code{\link{qdist}}, \code{\link{xpnorm}}, \code{\link{xqnorm}}.
#' @export
xpgamma <- function(...)  pdist("gamma", ...)
#' @rdname qdist
#' @export
xqgamma <- function(...)  qdist("gamma", ...)

#' @rdname pdist
#' @export
xpt <- function(...)  pdist("t", ...)
#' @rdname qdist
#' @export
xqt <- function(...)  qdist("t", ...)

#' @rdname pdist
#' @export
xpchisq <- function(...)  pdist("chisq", ...)
#' @rdname qdist
#' @export
xqchisq <- function(...)  qdist("chisq", ...)

#' @rdname pdist
#' @export
xpf <- function(...)  pdist("f", ...)
#' @rdname qdist
#' @export
xqf <- function(...)  qdist("f", ...)

#' @rdname pdist
#' @export
xpbinom <- function(...)  pdist("binom", ...)
#' @rdname qdist
#' @export
xqbinom <- function(...)  qdist("binom", ...)

#' @rdname pdist
#' @export
xppois <- function(...)  pdist("pois", ...)
#' @rdname qdist
#' @export
xqpois <- function(...)  qdist("pois", ...)

#' @rdname pdist
#' @export
xpgeom <- function(...)  pdist("geom", ...)
#' @rdname qdist
#' @export
xqgeom <- function(...)  qdist("geom", ...)

#' @rdname pdist
#' @export
xpnbinom <- function(...)  pdist("nbinom", ...)
#' @rdname qdist
#' @export
xqnbinom <- function(...)  qdist("nbinom", ...)


is_discrete_dist <- function(dist, ... ) {
  q <- dpqrdist(dist, type="q", p=ppoints(100), ...) 
  length(q) * .9 >= length(unique(q))
} 