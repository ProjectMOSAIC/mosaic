
tryCatch(utils::globalVariables(c('densy','densx','dots')), 
         error=function(e) message('Looks like you should update R.'))

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
#' @param add a logical indicating whether the plot should be added to the previous lattice plot. 
#' If missing, it will be set to match \code{under}.
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
#' @export
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
#' plotDist("norm", 10, 2, col="blue", type="h")
#' plotDist("norm", 12, 2, col="red", type="h", under=TRUE)
#' histogram( ~age|sex, data=HELPrct)
#' m <- mean( ~age|sex, data=HELPrct)
#' s <- sd(~age|sex, data=HELPrct)
#' plotDist( "norm", mean=m[1], sd=s[1], add=TRUE, packets=1)
#' plotDist( "norm", mean=m[2], sd=s[2], add=TRUE, packets=2)
#' 
#' @keywords graphics 
#' @keywords stats 
#' 

# utility for various graphical representations of distributions.

plotDist <- function( 
  dist, ...,
  add,
  under = FALSE,
  packets=NULL,
  rows=NULL,
  columns=NULL,
  kind = c('density','cdf','qq','histogram'), 
	xlab = "", ylab = "", breaks = NULL, type, 
	resolution = 5000,  params = NULL ) {
  
	kind = match.arg(kind)
  if (missing(add)) add <- under
	ddist = paste('d', dist, sep='')
	qdist = paste('q', dist, sep='')
	pdist = paste('p', dist, sep='')
  
  
  if (is.null(params)) {
    params <- list(...)
    dparams <- c(unnamed(params) , named_among( params, names(formals(ddist))) )
    pparams <- c(unnamed(params) , named_among( params, names(formals(pdist))) )
    qparams <- c(unnamed(params) , named_among( params, names(formals(qdist))) )
  } else {
    dparams <- params
    pparams <- params
    qparams <- params
  }
  
	values = do.call(qdist, c(p=list(ppoints(resolution)), qparams)) 
	fewerValues = unique(values)
	discrete = length(fewerValues) < length(values) 
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
    dots <- named(list(...))
    densx <- fewerValues
	  densy = do.call( ddist, c(list(x=fewerValues), dparams) ) 
#    print(names(as.list(environment())))
#    print(names(dots))
    
switch(kind, 
       density = 
         return( trellis.last.object() + latticeExtra::layer(
           do.call( lattice::panel.xyplot, 
                    c(list(x=densx, y=densy, type=type), dots) ),
           data = as.list(environment()),
           under=under,
           packets=packets,
           rows=rows,
           columns=columns) ),
       cdf = trellis.last.object() + latticeExtra::layer(
         do.call( lattice::panel.xyplot,  
                  c(list( x = cdfx, y = cdfy,  type=type), dots) ),
         data = list( cdfx=cdfx, cdfy=cdfy,  
                      type=type, dots=list(...) ),
         under=under,
         packets=packets,
         rows=rows,
         columns=columns),
       qq = 
         trellis.last.object() + latticeExtra::layer( 
           do.call( lattice::panel.qqmath,  
                    c(list(  x = values, type=type), dots) ),
           data= list(  values=values, type=type, dots=list(...) ), 
           under=under,
           packets=packets,
           rows=rows,
           columns=columns),
       histogram = 
         trellis.last.object() + latticeExtra::layer(  
           do.call( panel.xhistogram,  
                    c(list(x=values, type=type, breaks=breaks), dots)), 
           data = list(  values=values, 
                         breaks=breaks, type=type,
                         dots=list(...) ),
           under=under,
           packets=packets,
           rows=rows,
           columns=columns),
)
	} else {
	  switch(kind, 
	         density = 
	           lattice::xyplot( y ~ x, 
	                            data=data.frame( 
	                              y = do.call( ddist, c(list(x=fewerValues), dparams) ), 
	                              x = fewerValues), 
	                            type=type, xlab=xlab, ylab=ylab, ...),
	         cdf = 
	           lattice::xyplot( y ~ x, 
	                            data=data.frame( y = cdfy, x = cdfx ), 
	                            type=type, xlab=xlab, ylab=ylab, ...),
	         qq = 
	           lattice::qqmath( ~ x, 
	                            data = data.frame( 
	                              x = values, 
	                              y = do.call( ddist, c(list(x=values), dparams) ) ), 
	                            type=type, xlab=xlab, ylab=ylab, ...),
	         histogram = 
	           histogram( ~ x,
	                      data = data.frame( 
	                        x = values, 
	                        y = do.call( ddist, c(list(x=values), dparams) ) ), 
	                      type=type, xlab=xlab, breaks=breaks, ...)
	  )
	}
}

unnamed <-function(l)  if (is.null(names(l))) l else l [ names(l) == "" ]
named <-function(l)  if (is.null(names(l))) list() else l [ names(l) != "" ]
named_among <- function(l, n)  l [ intersect( names(l), n ) ]

