
distParams <- list(
  norm = list(mean=0, sd=1),
  t = list(df=NULL),
  chisq = list(df=NULL),
  f = list(df1=NULL, df2=NULL)
)

extractFromList <- function(l=list(), defaults) {
  res <- defaults
  for (item in intersect(names(defaults), names(l))) {
    res[[item]] <- l[[item]]  
  }
  return(res)
}

# remove from l1 named args that appear in l2 -- a bit list setdiff()
listdiff <- function(l1=list(), l2=list()) {
  res <- l1
  for (item in intersect(names(l1), names(l2))) {
    res[[item]] <- NULL
  }
  return(res)
}

dpqrdist <- function( dist, type=c("d","p","q","r"), ... ) {
  type <- match.arg(type)
  dots <- list(...)
  distFunName <- paste0(type,dist)
  do.call(distFunName, c(extractFromList(dots, distParams[[dist]]), 
                         listdiff(dots, distParams[[dist]])))
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
#' @param lower.tail a logical indicating whether lower tail probabilities are used
#' @param log.p a logical indicating whether the log of the probability should be 
#' calculated
#' @param xlim x limits
#' @param ylim y limits
#' @param vlwd width of vertical lines
#' @param vcol color of vertical lines
#' @param rot angle for rotating text indicating probability
#' @param ... additional arguments, including parameters of the distribution
#' and additional options for the plot
#' @return a vector of quantiles; often used for side effect of creating a plot
#' @examples
#' pdist("norm", -2:2)
#' pdist("norm", seq(80,120, by=10), mean=100, sd=10)
#' pdist("chisq", 2:4, df=3)
#' pdist("f", 1, df1=2, df2=10)
#' @export
 
pdist <- function (dist="norm", q, plot = TRUE, verbose = FALSE, invisible=FALSE, 
                   digits = 4, lower.tail = TRUE, log.p = FALSE, 
                   xlim, ylim,
                   vlwd=2, 
                   vcol=trellis.par.get('add.line')$col, 
                   rot=45, 
                   ...)
{
  if (! (dist %in% names(distParams)) ) {
    stop( paste0("I don't know how to deal with the `", dist, "' distribution") )
  }
  
  dots <- list(...)
  distParams <- distParams[[dist]]
  
  requiredDots <- names(distParams[is.null(distParams)])
  missingDots <- setdiff(requiredDots, names(dots)) 
  if (length(missingDots) > 0) {
    stop( paste("Missing parameters:", paste(missingDots, sep=", ")) )
  }
  
  p <- dpqrdist(dist, type="p", q, lower.tail = lower.tail, log.p = log.p, ...) 
  if (verbose) {
    cat("Verbose output not yet implemented.\n")
  }
  
  if (plot) {
    print(plot_multi_dist(dist=dist, p=p, 
                          xlim=xlim, ylim=ylim, 
                          digits=digits, 
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
#' @param lower.tail a logical indicating whether lower tail probabilities are used
#' @param log.p a logical indicating whether the log of the probability should be 
#' calculated
#' @param xlim x limits
#' @param ylim y limits
#' @param vlwd width of vertical lines
#' @param vcol color of vertical lines
#' @param rot angle for rotating text indicating probability
#' @param ... additional arguments, including parameters of the distribution
#' and additional options for the plot
#' @return a vector of probabilities; often used for side effect of creating a plot
#' @examples
#' qdist("norm", seq(.2, .8, by=.10))
#' qdist("norm", seq(.2, .8, by=.10), mean=100, sd=10)
#' qdist("chisq", .5,  df=3)
#' @export 

qdist <- function (dist="norm", p, plot = TRUE, verbose = FALSE, invisible=FALSE, 
                   digits = 4, lower.tail = TRUE, log.p = FALSE, 
                   xlim, ylim,
                   vlwd=2, 
                   vcol=trellis.par.get('add.line')$col, 
                   rot=45, 
                   ...)
{
  if (! (dist %in% names(distParams)) ) {
    stop( paste0("I don't know how to deal with the `", dist, "' distribution") )
  }
  
  dots <- list(...)
  distParams <- distParams[[dist]]
  
  requiredDots <- names(distParams[is.null(distParams)])
  missingDots <- setdiff(requiredDots, names(dots)) 
  if (length(missingDots) > 0) {
    stop( paste("Missing parameters:", paste(missingDots, sep=", ")) )
  }
  
  q <- dpqrdist(dist, type="q", p, lower.tail = lower.tail, log.p = log.p, ...) 
  if (verbose) {
    cat("Verbose output not yet implemented.\n")
  }
  
  if (plot) {
    print(plot_multi_dist(dist=dist, p=p, 
                          xlim=xlim, ylim=ylim, 
                          digits=digits, 
                          vlwd=vlwd, vcol=vcol, rot=rot, ...))
  }
  if (invisible) { 
    return(invisible(p))
  }
  return(q)
}


plot_multi_dist <- function(dist, p, q, xlim, ylim, digits=4, dlwd=2, 
                            vlwd=2, vcol=trellis.par.get('add.line')$col,
                            rot=0, ...) 
{
  dots <- list(...)
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
  xdata <- seq(xlim[1], xlim[2], length.out=400)
  
  ydata <- dpqrdist(dist, type="d", xdata, ...)
  ymax <- max(ydata, na.rm=TRUE)
  if (missing(ylim)) {
    ylim = c(0, 1.4 * ymax)
  }
  
  groups <- sapply(xdata, function(x) {sum(x < q)})
  
  # this could be funny if limits don't span q
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
      panel.xyplot(x,y, type='l', lwd=dlwd, col="black")
    } 
  ), dots)
  )
  
  return(plot)
}

