
# Note: one of d, p, q, or n must be a **named** argument.  This allows other things to 
# be in ... without causing issues for the d/p/q/r function called.
#' Distribution wrapper
#' 
#' Utility function wrapping up the d/p/q/r distribution functions
#' 
#' @param dist a character discription of a distribution, for example 
#'   \code{"norm"}, \code{"t"}, or \code{"chisq"}
#' @param type one of \code{"x"}, \code{"p"}, \code{"q"}, or \code{"r"}
#' @param ... additional arguments passed on to underlying distribution function.
#'   Note that one of \code{d}, \code{p}, \code{q}, or \code{n} must 
#'   be a named argument in ...
#' @export
#' @examples
#' # 3 random draws from N(1,2)
#' dpqrdist("norm", "r", n=3, mean = 1, sd = 2)
#' # These should all be the same
#' dpqrdist("norm", "d", x=0) == dnorm(x=0)
#' dpqrdist("norm", "p", q=0, mean = 1, sd = 2) == pnorm(q=0, mean = 1, sd = 2)
#' dpqrdist("norm", "q", p=0.5, mean = 1, sd = 2) == qnorm(p=0.5, mean = 1, sd = 2)
#' 
dpqrdist <- function( dist, type=c("d","p","q","r"), ... ) {
  type <- match.arg(type)
  dots <- list(...)
  distFunName <- paste0(type, dist)
  distdots <- dots[names(dots) %in% names(formals(distFunName))]

  do.call(distFunName, distdots)
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
#' @param resolution number of points used for detecting discreteness and generating plots.  
#'        The defaut value of 5000 should work well except for discrete distributions
#'        that have many distinct values, especially if these values are not evenly spaced.
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
                   digits = 4L, 
                   xlim, ylim,
                   vlwd=NULL, 
                   vcol=trellis.par.get('add.line')$col, 
                   rot=45, 
                   resolution = 5000L,
                   ...)
{
  
  dots <- list(...)
  
  if (is.null(vlwd)) {
    vlwd <- if (is_discrete_dist( dist, ... )) 0 else 2
  }
  
  p <- dpqrdist(dist, type="p", q=q, ...) 
  
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
#' @param xlim x limits.  By default, these are chosen to show the central 99.8\% 
#'   of the distribution.
#' @param ylim y limits
#' @param vlwd width of vertical lines
#' @param vcol color of vertical lines
#' @param rot angle for rotating text indicating probability
#' @param resolution number of points used for detecting discreteness and generating plots.  
#'        The defaut value of 5000 should work well except for discrete distributions
#'        that have many distinct values, especially if these values are not evenly spaced.
#' @param ... additional arguments, including parameters of the distribution
#' and additional options for the plot
#' @details The most general function is \code{qdist} which can work with 
#' any distribution for which a q-function exists.  As a convenience, wrappers are 
#' provided for several common distributions.
#' 
#' @return a vector of quantiles; a plot is printed as a side effect
#' @examples
#' qdist("norm", seq(.2, .8, by = 0.10))
#' xqnorm(seq(.2, .8, by = 0.10), mean = 100, sd = 10)
#' qdist("unif", .5)
#' xqgamma(.5, shape = 3, scale = 4)
#' xqchisq(c(.25,.5,.75), df = 3)
#' xpbinom( c(480, 500, 510), size = 1000, prob = 0.48)
#' xpbinom( c(40, 60), size = 100, prob = 0.5)
#' xqpois( c(0.25, 0.5, 0.75), lambda = 6, lwd = 3, vlwd = 2)
#' @export 

qdist <- function (dist="norm", p, plot = TRUE, verbose = FALSE, invisible=FALSE, 
                   resolution = 5000L,
                   digits = 4L, 
                   xlim, ylim,
                   vlwd=NULL, 
                   vcol=trellis.par.get('add.line')$col, 
                   rot=45, 
                   ...)
{
  
  dots <- list(...)
  
  if (is.null(vlwd)) {
    vlwd <- if (is_discrete_dist( dist, ... )) 0 else 2
  }
  
  q <- dpqrdist(dist, type="q", p=p, ...) 
  # adjust probs for discrete dists where requested p's might not be (exactly) possible
  p <- dpqrdist(dist, type="p", q=q, ...)  
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


plot_multi_dist <- 
  function(dist, p, q, xlim, ylim, digits=4, resolution=5000L,
           dlwd=2, vlwd=if (discrete) 0 else 2, vcol=trellis.par.get('add.line')$col,
           rot=0, ...) 
{
  dots <- list(...)
  latticedots <- dots[ ! names(dots) %in% names(formals(paste0("p",dist))) ]
  distdots <- dots[ names(dots) %in% names(formals(paste0("p",dist))) ]
  
  discrete <- do.call(is_discrete_dist, c(list(dist), distdots))
  
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
  
  if (! 'lty' %in% names(latticedots)) { latticedots$lty <- 1 }
  if (! 'pch' %in% names(latticedots)) { latticedots$pch <- 16 }
  
  if (missing(xlim) || is.null(xlim)) {
    xlim <- range(dpqrdist(dist, type="q", p=c(0.001, .999), ...))
  }
  if (discrete) {
    xdata <- unique(dpqrdist(dist, type="q", p=ppoints(resolution), ...))
    step = min(diff(xdata))
    xlim = c(-step/2, step/2) + xlim  # widen by half step in each direction
    fill <- seq( min(xdata) -1.5 * step , max(xdata) + 1.5*step, length.out=resolution)
    xdata <- c(xdata, fill) 
    xdata <- dpqrdist(dist, type="q", p=dpqrdist(dist, type="p", q=xdata, ...), ...)
    xdata <- sort(unique(xdata))
  } else {
    xdata <- unique(seq(xlim[1], xlim[2], length.out=resolution))
  }
 
  # not a fix: xdata <- sort(xdata) 
  ydata <- dpqrdist(dist, type="d", x=xdata, ...)
  ymax <- 1.1 * quantile(ydata, 0.95, na.rm=TRUE)
  if (missing(ylim)) {
    ylim = c(0, 1.4 * ymax)  # quantile(ydata, 0.95, na.rm = TRUE))
    print(ylim)
  }
  
  # this could be funny if limits don't span q
  p <- c(0, p, 1)
  q <- c(xlim[1], q, xlim[2])
  
  if (discrete) {
    if (is.null(latticedots$pch)) latticedots$pch = 16
  }
  
  args <- c(
    list(
      ydata ~ xdata, 
      xlim = xlim, ylim = ylim, 
      groups = sapply(xdata, function(x) {sum(x <= q)}),
      type= if (discrete) c('p','h') else 'h',
      xlab = "", ylab = if (discrete) "probability" else "density", 
      panel = 
        if (discrete) {
          function(x, y, ...) {
            # panel.xyplot(x,y,...)
            panel.segments(q, 0, q, grid::unit(ymax,'native') + grid::unit(.2,'lines'), 
                           col = vcol, lwd=vlwd)
            grid.text(x=mid(q), y=grid::unit(ymax,'native') + grid::unit(1.0,'lines'), default.units='native',
                      rot=rot,
                      check.overlap=TRUE,
                      paste("", round(diff(p), 3), sep = ""), 
                      just = c('center','center'),  gp=gpar(cex = 1.0))
            panel.xyplot(x, y, ...)
          } 
        } else {
          function(x, y, ...) {
            panel.xyplot(x, y, ...)
            panel.segments(q, 0, q, grid::unit(ymax,'native') + grid::unit(.2,'lines'), 
                           col = vcol, lwd=vlwd)
            grid.text(x=mid(q), y=grid::unit(ymax,'native') + grid::unit(1.0,'lines'), default.units='native',
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

#' @rdname pdist
#' @export
xpbeta <- function(...)  pdist("beta", ...)
#' @rdname qdist
#' @export
xqbeta <- function(...)  qdist("beta", ...)


is_discrete_dist <- function(dist, ... ) {
  q <- dpqrdist(dist, type="q", p=ppoints(100), ...) 
  length(q) * .9 >= length(unique(q))
} 
