utils::globalVariables(c('densy','densx','dots')) 

#' Plots of Discrete and Continuous Distributions
#' 
#' Provides a simple way to generate plots of pdfs, probability mass functions,
#' cdfs, probability histograms, and normal-quantile plots for distributions
#' known to R.
#'
#' @param dist 
#' 	  A string identifying the distribution.  This should work
#' 	  with any distribution that has associated functions beginning
#' 	  with 'd', 'p', and 'q' (e.g, 
#' 	  \code{\link{dnorm}},
#' 	  \code{\link{pnorm}}, and 
#' 	  \code{\link{qnorm}}).  \code{dist} should match the name of the 
#' 	  distribution with the initial 'd', 'p', or 'q' removed.
#' @param xlim a numeric vector of length 2 or \code{NULL}, in which case
#'  the central 99.8\% of the distribution is used.
#' @param ylim a numeric vector of length 2 or \code{NULL}, in which case
#'  a heuristic is used to avoid chasing asymptotes in distributions like
#'  the F distributions with 1 numerator degree of freedom.
#' @param add a logical indicating whether the plot should be added to the previous lattice plot.
#'   If missing, it will be set to match \code{under}.
#' @param under a logical indicating whether adding should be done in a layer under or over the existing 
#' layers when \code{add = TRUE}.
#' @param packets,rows,columns specification of which panels will be added to when 
#' \code{add} is \code{TRUE}.  See \code{\link[latticeExtra]{layer}}.
#' @param params a list containing parameters for the distribution.  If \code{NULL} (the default), 
#' this list is created from elements of \code{\dots} that are either unnamed or have names among
#' the formals of the appropriate distribution function.  See the examples.
#' @param kind one of "density", "cdf", "qq", or "histogram" (or prefix 
#' 	  of any of these)
#' @param xlab,ylab as per other lattice functions
#' @param breaks  a vector of break points for bins of histograms,
#'    as in \code{\link{histogram}} 
#' @param type  passed along to various lattice graphing functions 
#' @param resolution  number of points to sample when generating the plots
#' @param \dots  other arguments passed along to lattice graphing routines
#' 
#' @details
#' 	\code{plotDist} determines whether the distribution 
#' 	is continuous or discrete by seeing if all the sampled quantiles are 
#' 	unique.  A discrete random variable with many possible values could 
#' 	fool this algorithm and be considered continuous.
#' 
#' The plots are done referencing a data frame with variables
#' \code{x} and \code{y} giving points on the graph of the 
#' pdf, pmf, or cdf for the distribution.  This can be useful in conjuction
#' with the \code{groups} argument.  See the examples.
#' 
#' @examples
#' plotDist('norm')
#' plotDist('norm', type='h')
#' plotDist('norm', kind='cdf')
#' plotDist('exp',  kind='histogram')
#' plotDist('binom', params=list( 25, .25))       # explicit params
#' plotDist('binom', 25, .25)                     # params inferred
#' plotDist('norm', mean=100, sd=10, kind='cdf')  # params inferred
#' plotDist('binom', 25, .25, xlim=c(-1,26) )     # params inferred
#' plotDist('binom', params=list( 25, .25), kind='cdf')
#' plotDist('beta', params=list( 3, 10), kind='density')
#' plotDist('beta', params=list( 3, 10), kind='cdf')
#' plotDist( "binom", params=list(35,.25), 
#'            groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25) )
#' plotDist( "binom", params=list(35,.25), 
#'            groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25), 
#'            kind='hist')
#' plotDist("norm", mean=10, sd=2, col="blue", type="h")
#' plotDist("norm", mean=12, sd=2, col="red", type="h", under=TRUE)
#' plotDist("binom", size=100, prob=.30) +
#'   plotDist("norm", mean=30, sd=sqrt(100 * .3 * .7))
#' plotDist("chisq", df=4, groups = x > 6, type="h")
#' plotDist("f", df1=1, df2 = 99)
#' if (require(mosaicData)) {
#' histogram( ~age|sex, data=HELPrct)
#' m <- mean( ~age|sex, data=HELPrct)
#' s <- sd(~age|sex, data=HELPrct)
#' plotDist( "norm", mean=m[1], sd=s[1], col="red", add=TRUE, packets=1)
#' plotDist( "norm", mean=m[2], sd=s[2], col="blue", under=TRUE, packets=2)
#' }
#' 
#' @keywords graphics 
#' @keywords stats 
#' @export

plotDist <- function( 
  dist, ...,
  xlim = NULL,
  ylim = NULL,
  add,
  under = FALSE,
  packets=NULL,
  rows=NULL,
  columns=NULL,
  kind = c('density','cdf','qq','histogram'), 
  xlab = "", ylab = "", breaks = NULL, type, 
  resolution = 5000L,  params = NULL ) {
  
  kind = match.arg(kind)
  if (missing(add)) add <- under
  ddist = paste('d', dist, sep='')
  qdist = paste('q', dist, sep='')
  pdist = paste('p', dist, sep='')
  
  original_call <- match.call()  
  dots <- original_call
  dots[[1]] <- NULL
  unnamed_dots <- original_call
  named_dots <- original_call
  unnamed_dots[[1]] <- NULL
  named_dots[[1]] <- NULL
  groupless_dots <- original_call
  groupless_dots[[1]] <- NULL
  for (i in length(unnamed_dots):1) {
    if (names(unnamed_dots)[i] != "") {
      unnamed_dots[i] <- NULL
    } else {
      named_dots[i] <- NULL
    }
  }
  if (is.null(params)) {
    params <- original_call
    params[[1]] <- NULL
    for (item in names(formals()) ) {
      if (item %in% names(params)) params[[item]] <- NULL
    }
    dparams <- c(unnamed(params) , named_among( params, names(formals(ddist))) )
    pparams <- c(unnamed(params) , named_among( params, names(formals(pdist))) )
    qparams <- c(unnamed(params) , named_among( params, names(formals(qdist))) )
  } else {
    dparams <- params
    pparams <- params
    qparams <- params
  }
  # attempting to make evaluation of these arguments more intuitive 
  env <- parent.frame()
  dparams <- lapply(dparams, function(x) eval(x, env))
  pparams <- lapply(pparams, function(x) eval(x, env))
  qparams <- lapply(qparams, function(x) eval(x, env))
  
  values = do.call(qdist, c(p=list(ppoints(resolution)), qparams)) 

  fewerValues <- unique(values)
  discrete = length(fewerValues) < length(values) 
  
  if (! discrete) {
    values = seq(
      do.call(qdist, c(p = list(0.001), qparams)),
      do.call(qdist, c(p = list(0.999), qparams)),
      length.out = resolution
    ) 
    fewerValues <- values
  }
  
  if ( is.null(breaks) && discrete ){
    step = min(diff(fewerValues))
    breaks = seq( min(fewerValues) -.5 * step , max(fewerValues) + .5*step, step)
  }
  
  if (kind=='cdf') {
    if (discrete) {
      step = min(diff(fewerValues))
      cdfx <- seq( min(fewerValues) -1.5 * step , max(fewerValues) + 1.5*step, length.out=resolution)
      cdfx <- sort(unique( c(fewerValues, cdfx) ) )
      cdfy <- approxfun( fewerValues, do.call(pdist, c(list(q=fewerValues),pparams)), method='constant', 
                         f=0, yleft=0, yright=1 ) (cdfx)
    } else {
      cdfx <- values
      cdfy <- do.call( pdist, c(list(q=values), pparams) ) 
    }
  }
  if (missing(type)) {
    if (discrete) {
      type = switch(kind,
                    density = c('p','h'),
                    cdf = 'p',
                    histogram = 'density',
                    qq = 'l')  
    } else {
      type = switch(kind,
                    density = 'l',
                    cdf = 'l',
                    histogram = 'density',
                    qq = 'l')
    }
  }
  
  if (add) {
    call_without_add <- original_call
    call_without_add["add"] <- FALSE
    return( 
      trellis.last.object() + 
        latticeExtra::as.layer(
          eval.parent(call_without_add),
          under=under,
          packets=packets,
          rows=rows,
          columns=columns) 
    )
  } else { # not adding
    if (is.null(xlim)) {
      xlim <- do.call(qdist, c(list(p=c(0.001, .999)), qparams))
    }
    ydata <- 
      switch(kind,
             density = do.call(ddist, c(list(x=fewerValues), dparams)),
             cdf = cdfy,
             qq = do.call( ddist, c(list(x=values), dparams)),
             histogram = do.call(ddist, c(list(x=values), dparams))
      )
    if (is.null(ylim)) {
      ymax <- 
        min(
          1.6 * quantile(ydata, 0.90, na.rm=TRUE),
          1.1 * max(ydata, na.rm=TRUE),
          na.rm = TRUE)
      ylim = c(0, ymax)  
    }
    
    switch(kind, 
           density = 
             lattice::xyplot( y ~ x, 
                              data=data.frame(y = ydata, x = fewerValues), 
                              xlim = xlim, ylim = ylim,
                              type=type, xlab=xlab, ylab=ylab, ...),
           cdf = 
             lattice::xyplot( y ~ x, 
                              data=data.frame(y = ydata, x = cdfx), 
                              xlim = xlim, ylim = ylim,
                              type=type, xlab = xlab, ylab = ylab, ...),
           qq = 
             lattice::qqmath( ~ x, 
                              data = data.frame(x = values, y = ydata),
                              xlim = xlim, ylim = ylim,
                              type = type, xlab = xlab, ylab = ylab, ...),
           histogram = 
             histogram( ~ x,
                        data = data.frame(x = values,  y = ydata),
                        xlim = xlim, ylim = ylim,
                        type = type, xlab = xlab, breaks = breaks, ...)
    )
  }
}

#' List extraction
#' 
#' These functions create subsets of lists based on their names
#'
#'  
#' @param l a list
#' @param n a vector of character strings (potential names)
#' @return a sublist of \code{l} determined by \code{names(l)}
#' @export

named <-function(l)  if (is.null(names(l))) list() else l [ names(l) != "" ]

#' @rdname named
#' @export

unnamed <-function(l)  if (is.null(names(l))) l else l [ names(l) == "" ]

#' @rdname named
#' @export

named_among <- function(l, n)  l [ intersect( names(l), n ) ]

