#' The Data Distribution
#' 
#' Density, distribution function, quantile function, and random generation
#' from data.
#'
#' 
#' @export
#' 
#' @keywords distribution 
#' 

#  First draft: Written June 14, 2009

#' Check whether input is quantitative
#'
#' @rdname undocked
#' @param x a vector
#' @return TRUE if quantitative; else \code{stop}
#'
.check_for_quant_input <- function(x) {
	if (is.data.frame(x) ) {
		stop("Give a variable, not a data frame.")
	}

	if (is.factor(x) | is.character(x) ) {
		stop("Input must be a numerical variable, not categorical.")
	}
	return(TRUE)
}

#' Compute quantiles from data
#'
#' \code{qdata} is a wrapper around \code{\link{quantile}} that makes the syntax more like 
#' the syntax for quantiles from theoretical distributions
#'
#' @param p a vector of probabilities
#' @param vals a vector containing the data
#' @param data a data frame in which to evaluate vals
#' @param \dots additional arguments passed to \code{quantile} or \code{sample}
#' @return For \code{qdata}, a vector of quantiles
#' @rdname pqrdata
#'
#' @examples
#' data(iris)
#' qdata(.5, iris$Sepal.Length)
#' qdata(.5, Sepal.Length, data=iris)

qdata <- function(p, vals, data=NULL, ... ) {
        if( !is.null(data) ) { # handle data= style of passing values
            vals = eval( substitute(vals), data, enclos=parent.frame())
        }
	.check_for_quant_input(vals)
	if (any(p > 1) | any(p < 0) ) {
		stop("Prob outside of range 0 to 1.  Do you perhaps want pdata?")
	}
	quantile(vals, probs=p, na.rm=TRUE, ... )
}


#' \code{pdata} computes cumulative probabilities from data.
#'
#' @param q a vector of quantiles
#' @param lower.tail a logical indicating whether to use the lower or upper tail probability
#' @return For \code{pdata}, a vector of probabilities
#' @rdname pqrdata
#' @examples
#' data(iris)
#' pdata(3:6, iris$Sepal.Length)
#' pdata(3:6, Sepal.Length, data=iris)
#'
pdata = function(q, vals, data=NULL, lower.tail=TRUE, ... ) {
   if( !is.null(data) ) {
         vals = eval( substitute(vals), data, enclos=parent.frame())
   }
  .check_for_quant_input(vals)
#  L = length(q)
#  res = rep(0,L)
  n <- sum( ! is.na(vals) )
  probs <- sapply( q, function(q) { sum( vals <= q , na.rm=TRUE ) } ) / n
  if (lower.tail) { 
  	return(probs)
  } else {
	return( 1 - probs )
  }
}

#' \code{rdata} randomly samples from data. It is a wrapper around \code{sample} that unifies syntax.
#'
#' @param n number of values to sample
#' @param replace  a logical indicating whether to sample with replacement
#' @return For \code{rdata}, a vector of values sampled from \code{vals} 
#' @rdname pqrdata
#'
#' @examples
#' data(iris)
#' rdata(10,iris$Species)
#' rdata(10, Species, data=iris)
#'

rdata = function(n, vals, data=NULL, replace=TRUE, ... ) {
   if( !is.null(data) ) {
         vals = eval( substitute(vals), data, enclos=parent.frame())
   }
  sample( vals, n, replace=replace, ...)
}


#' \code{ddata} computes a probability mass function from data.
#'
#' @param x a vector of quantiles
#' @param log  a logical indicating whether the result should be log transformed
#' @return For \code{ddata}, a vector of probabilities (empirical densities)
#' @rdname pqrdata
#' @examples
#' data(iris)
#' ddata('setosa', iris$Species)
#' ddata('setosa', Species, data=iris)
#'

ddata = function(x, vals, data=NULL, log=FALSE, ...) {
        if( !is.null(data) ) {
         vals = eval( substitute(vals), data, enclos=parent.frame())
        }
	n <- sum( ! is.na(vals) )
	probs <- sapply(x, function(x) { sum( vals == x, na.rm=TRUE ) / n } )
	if (log) { probs <- log(probs) }
	return (probs)
}

