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
#' @param params a list containing parameters for the distribution
#' @param kind one of "density", "cdf", "qq", or "histogram" (or prefix 
#' 	  of any of these)
#' @param xlab,ylab as per other lattice functions
#' @param breaks  a vector of break points for bins of histograms,
#'    as in \code{\link{xhistogram}} 
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
#' pdf or pmf for the distribution.  This can be useful in conjuction
#' with the \code{groups} argument.  See the examples.
#' 
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' @export
#' @examples
#' plotDist('norm')
#' plotDist('norm', type='h')
#' plotDist('norm', kind='cdf')
#' plotDist('norm', params=list(mean=100, sd=10), kind='cdf')
#' plotDist('exp', kind='histogram')
#' plotDist('binom', params=list( 25, .25))
#' plotDist('binom', params=list( 25, .25), xlim=c(-1,26) )
#' plotDist('binom', params=list( 25, .25), kind='cdf')
#' plotDist('beta', params=list( 3, 10), kind='density')
#' plotDist('beta', params=list( 3, 10), kind='cdf')
#' plotDist( "binom", params=list(35,.25), groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25) )
#' plotDist( "binom", params=list(35,.25), groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25), kind='hist')
#' 
#' @keywords graphics 
#' @keywords stats 
#' 

# utility for various graphical representations of distributions.

plotDist <- function( dist, params=list(), kind=c('density','cdf','qq','histogram'), 
					 xlab="", ylab="", breaks=NULL, type, 
					 resolution=5000,... ) {
	kind = match.arg(kind)
	ddist = paste('d', dist, sep='')
	qdist = paste('q', dist, sep='')
	pdist = paste('p', dist, sep='')

	values = do.call(qdist, c(p=list(ppoints(resolution)), params)) 
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
			cdfy <- approxfun( fewerValues, do.call(pdist, c(list(q=fewerValues),params)), method='constant', 
							  f=1, yleft=0, yright=1 ) (cdfx)
		} else {
			cdfx <- values
			cdfy <- do.call( pdist, c(list(q=values), params) ) 
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

	switch(kind, 
		density = 
			lattice::xyplot( y ~ x, 
				data=data.frame( 
					y = do.call( ddist, c(list(x=fewerValues), params) ), 
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
					y = do.call( ddist, c(list(x=values), params) ) ), 
				type=type, xlab=xlab, ylab=ylab, ...),
		histogram = 
			xhistogram( ~ x,
				data = data.frame( 
					x = values, 
					y = do.call( ddist, c(list(x=values), params) ) ), 
				type=type, xlab=xlab, breaks=breaks, ...)
		   )
}

