
#' The Data Distribution
#' 
#' Density, distribution function, quantile function, and random generation
#' from data.

#' @rdname pqrdata
#' @keywords distribution 
#' @param p a vector of probabilities
#' @param q a vector of quantiles
#' @param formula a formula or a vector
#' @param data a data frame in which to evaluate \code{formula}
#' @param \dots additional arguments passed to \code{quantile} or \code{sample}
#' @return For \code{qdata}, a vector of quantiles
#' @examples
#' data(iris)
#' qdata(Sepal.Length ~ Species, 0.5, data=iris)
#' qdata(~Sepal.Length, p = 0.5, groups=Species, data=iris)
#' qdata(iris$Sepal.Length, p = 0.5)
#' qdata(Sepal.Length, p = 0.5, data=iris)
#' qdata(Sepal.Length, p = 0.5, groups=Species, data=iris)
#' @export

qdata <- function( formula, p = seq(0, 1, 0.25), data = NULL, ...) { 
  vals_call <- substitute(formula)
  if (! is.numeric(p) || any(p < 0) || any(p > 1)) {
    stop("Invalid values of p.  Are your arguments in the wrong order?")
  }
  args <- eval(substitute(alist(vals_call, ...)))
  args [["p"]] <- p 
  args [["data"]] <- data
  do.call( "qdata_f", args )
}


#' The Data Distribution
#' 
#' Utility functions for density, distribution function, quantile function, 
#' and random generation from data.
#' 
#' @rdname pqrdata2
#' @seealso  \code{\link{ddata}}, \code{\link{pdata}}, \code{\link{qdata}},  
#' \code{\link{rdata}}, \code{\link{cdata}}
#' @param p a vector of probabilities
#' @param q a vector of quantiles
#' @param x a vector containing the data
#' @param data a data frame in which to evaluate \code{formula}
#' @param n number of values to sample
#' @param replace  a logical indicating whether to sample with replacement
#' @param groups a grouping variable, typically the name of a variable in \code{data}
#' @param \dots additional arguments passed to \code{quantile} or \code{sample}
#' @param na.rm a logical indicating whether \code{NA}s should be removed before computing.
#' @param log  a logical indicating whether the result should be log transformed
#' @export
 
qdata_v <- function( x, p=seq(0, 1, 0.25), na.rm=TRUE, ... ) {
  .check_for_quant_input(x)
  if ( any(p > 1) | any(p < 0) ) 
    stop("Prob outside of range 0 to 1.  Do you perhaps want pdata?")
  qs <- quantile(x, probs=p, na.rm=na.rm, ... )
  if (length(p) == 1) {
    result <- setNames( c(p, qs), c("p","quantile") )
  } else {
    result <- data.frame(quantile = qs, p=p)
  }
  result
}

#' @rdname pqrdata2
#' @export

qdata_f <- aggregatingFunction1(qdata_v, output.multiple = TRUE, na.rm = TRUE)

#' @rdname pqrdata
#' @return for \code{cdata}, a named numerical vector or a data frame giving
#' upper and lower limits and the central proportion requested
#' @examples
#' data(iris)
#' cdata(iris$Sepal.Length, 0.5)
#' cdata(Sepal.Length, 0.5, data = iris)
#' cdata( ~ Sepal.Length, 0.5, data = iris)
#' cdata( ~ Sepal.Length | Species, data = iris, p = .5)
#' @export

cdata <- function( formula, p = 0.95, data = NULL, ...) { 
  vals_call <- substitute(formula)
  if (! is.numeric(p) || any(p < 0) || any(p > 1)) {
    stop("Invalid values of p.  Are your arguments in the wrong order?")
  }
  
  args <- eval(substitute(alist(vals_call, ...)))
  args [["p"]] <- p # substitute(p, parent.frame())
  args [["data"]] <- data
  do.call( "cdata_f", args )
}
#' @rdname pqrdata2
#' @export

cdata_v <- function( x, p = .95, na.rm = TRUE, ... ) {
  lo_p <- (1-p)/2
  hi_p <- 1 - lo_p
  lo <- quantile( x, lo_p, na.rm=na.rm, ... )
  hi <- quantile( x, hi_p, na.rm=na.rm, ... )
  if (length(p) == 1) {
    result <- setNames( c(lo, hi, p), c("low", "hi", "central.p") )
  } else {
    result <- data.frame(low=lo, hi=hi, central.p=p)
  }
  return(result)
}

#' @rdname pqrdata2
#' @export

cdata_f <- aggregatingFunction1( cdata_v, output.multiple = TRUE, na.rm = TRUE )

  
#' @rdname pqrdata
#' @return For \code{pdata}, a vector of probabilities
#' @examples
#' data(iris)
#' pdata(iris$Sepal.Length, 3:6)
#' pdata(Sepal.Length, 3:6, data=iris)
#' pdata(~Sepal.Length, 3:6, data=iris)
#' pdata(Sepal.Length, 3:6, data=iris)
#' @export

pdata <- function (formula, q, data = NULL, ...) 
{
  vals_call <- substitute(formula)
  args <- eval(substitute(alist(vals_call, ...)))
  args[["q"]] <- q
  args[["data"]] <- data
  do.call("pdata_f", args)
}



#' @param lower.tail a logical indicating whether to use the lower or upper tail probability
#' @rdname pqrdata2
#' @export

pdata_v <- function(x, q, lower.tail=TRUE, ... ) {
  .check_for_quant_input(x)
  n <- sum( ! is.na(x) )
  probs <- sapply( q, function(q) { sum( x <= q , na.rm=TRUE ) } ) / n
  if (lower.tail) { 
  	return(probs)
  } else {
	return( 1 - probs )
  }
}

#' @rdname pqrdata2
#' @export

pdata_f <- aggregatingFunction1( pdata_v, output.multiple=TRUE, na.rm=TRUE )



#' @rdname pqrdata
#' @param n number of values to sample
#' @return For \code{rdata}, a vector of sampled values.
#' @examples
#' data(iris)
#' rdata(iris$Species, 10)
#' rdata(Species, n = 10, data=iris)
#' rdata(~Species, n = 10, data=iris)
#' rdata(Sepal.Length~Species,  n = 5, data=iris)
#' @export

rdata <- function (formula, n, data = NULL, ...) 
{
  vals_call <- substitute(formula)
  if ( ! is.numeric(n) || n <= 0 ) {
    stop("Invalid value of n.  Are our arguments in the correct order?")
  }
  args <- eval(substitute(alist(vals_call, ...)))
  args[["n"]] <- n
  args[["data"]] <- data
  do.call("rdata_f", args)
}
 
#' @rdname pqrdata2
#' @export

rdata_v <- function(x, n, replace=TRUE, ... ) {
  sample( x, n, replace=replace, ...)
}

#' @rdname pqrdata2
#' @export

rdata_f <- aggregatingFunction1( rdata_v, output.multiple=TRUE, na.rm=TRUE )

#' @rdname pqrdata
#' @return For \code{ddata}, a vector of probabilities (empirical densities)
#' @examples
#' data(iris)
#' ddata(iris$Species, 'setosa')
#' ddata(Species, 'setosa', data=iris)
#' ddata(~Species, 'setosa', data=iris)
#' @export

ddata <- function (formula, q, data = NULL, ...) 
{
  vals_call <- substitute(formula)
  args <- eval(substitute(alist(vals_call, ...)))
  args[["q"]] <- q
  args[["data"]] <- data
  do.call("ddata_f", args)
}

#' @rdname pqrdata2
#' @export

ddata_v <- function(x, q, ..., data=NULL, log=FALSE, na.rm=TRUE) {
  if( !is.null(data) ) {
    vals = eval( substitute(x), data, enclos=parent.frame())
  }
  n <- sum( ! is.na(vals) )
  probs <- sapply(q, function(x) { sum( vals == x, na.rm=na.rm) / n } )
  if (log) { probs <- log(probs) }
  return (probs)
}

#' @rdname pqrdata2
#' @export

ddata_f <- aggregatingFunction1( ddata_v, output.multiple=TRUE, na.rm=TRUE )

.check_for_quant_input <- function(x) {
  if (is.data.frame(x) ) {
    stop("Give a variable, not a data frame.")
  }
  
  if (is.factor(x) | is.character(x) ) {
    stop("Input must be a numerical variable, not categorical.")
  }
  return(TRUE)
}
