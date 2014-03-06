

.check_for_quant_input <- function(x) {
	if (is.data.frame(x) ) {
		stop("Give a variable, not a data frame.")
	}

	if (is.factor(x) | is.character(x) ) {
		stop("Input must be a numerical variable, not categorical.")
	}
	return(TRUE)
}

#' The Data Distribution
#' 
#' Density, distribution function, quantile function, and random generation
#' from data.
#'
#' \code{qdata} is a wrapper around \code{\link{quantile}} that makes the syntax more like 
#' the syntax for quantiles from theoretical distributions

#'
#' @param p a vector of probabilities
#' @param vals a vector containing the data
#' @param data a data frame in which to evaluate vals
#' @param groups a grouping variable, typically the name of a variable in \code{data}
#' @param ..fun.. a function.  Most users will not need to change the default value.
#' @param \dots additional arguments passed to \code{quantile} or \code{sample}
#' @param na.rm a logical indicating whether \code{NA}s should be removed before computing.
#' @return For \code{qdata}, a vector of quantiles
#' @export
#' @rdname pqrdata
#' @keywords distribution 

# .qdata_old <- function(p, vals, data=NULL, ... ) {
#         if( !is.null(data) ) { # handle data= style of passing values
#             vals = eval( substitute(vals), data, enclos=parent.frame())
#         }
# 	.check_for_quant_input(vals)
# 	if (any(p > 1) | any(p < 0) ) {
# 		stop("Prob outside of range 0 to 1.  Do you perhaps want pdata?")
# 	}
# 	quantile(vals, probs=p, na.rm=TRUE, ... )
# }

#' @rdname pqrdata
#' @examples
#' data(iris)
#' qdata(.5, Sepal.Length ~ Species, data=iris)
#' qdata(.5, ~Sepal.Length, groups=Species, data=iris)
#' qdata(.5, iris$Sepal.Length)
#' qdata(.5, Sepal.Length, data=iris)
#' qdata(.5, Sepal.Length, groups=Species, data=iris)


#' @rdname pqrdata
#' 
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

#' @rdname pqrdata
qdata_f <- aggregatingFunction1(qdata_v, output.multiple=TRUE, na.rm=TRUE)

#' @rdname pqrdata
qdata <- function( p, vals, data=NULL, ...) { 
  vals_call <- substitute(vals)
  args <- eval(substitute(alist(vals_call, ...)))
  args [["p"]] <- p 
  args [["data"]] <- data
  do.call( "qdata_f", args )
}


# .cdata_old <- function( p, vals, data=NULL, ...) {
#   if( !is.null(data) ) { # handle data= style of passing values
#     vals = eval( substitute(vals), data, enclos=parent.frame())
#   }
#   lo_p <- (1-p)/2
#   hi_p <- 1 - (1-p)/2
#   lo <- qdata( lo_p, vals, data=data, ...)
#   hi <- qdata( hi_p, vals, data=data, ...)
#   result <- cbind(low=lo, hi=hi)
#   row.names(result) <- paste( 100*p, "%", sep="" )
#   return(result)
# }
# 

#' @rdname pqrdata
#' @export
#' @examples
#' data(iris)
#' cdata(.5, iris$Sepal.Length)
#' cdata(.5, Sepal.Length, data=iris)
#' cdata_f(~Sepal.Length, data=iris, p=.5)
#' cdata_f(~Sepal.Length | Species, data=iris, p=.5)
 
cdata_v <- function( x, p=.95, na.rm=TRUE, ... ) {
  lo_p <- (1-p)/2
  hi_p <- 1 - lo_p
  lo <- quantile( x, lo_p, na.rm=na.rm, ... )
  hi <- quantile( x, hi_p, na.rm=na.rm, ... )
  if (length(p) == 1) {
    result <- setNames( c(lo, hi, p), c("low", "hi", "central.p") )
  } else {
    result <- data.frame(low=lo, hi=hi, central.p=p)
  }
  # row.names(result) <- paste( 100*p, "%", sep="" )
  return(result)
}

#' @rdname pqrdata
cdata_f <- aggregatingFunction1( cdata_v, output.multiple=TRUE, na.rm=TRUE )


#' @rdname pqrdata
cdata <- function( p, vals, data=NULL, ...) { 
  vals_call <- substitute(vals)
  args <- eval(substitute(alist(vals_call, ...)))
  args [["p"]] <- p # substitute(p, parent.frame())
  args [["data"]] <- data
  do.call( "cdata_f", args )
}
  
# .pdata = function(q, vals, data=NULL, lower.tail=TRUE, ... ) {
#    if( !is.null(data) ) {
#          vals = eval( substitute(vals), data, enclos=parent.frame())
#    }
#   .check_for_quant_input(vals)
# #  L = length(q)
# #  res = rep(0,L)
#   n <- sum( ! is.na(vals) )
#   probs <- sapply( q, function(q) { sum( vals <= q , na.rm=TRUE ) } ) / n
#   if (lower.tail) { 
#   	return(probs)
#   } else {
# 	return( 1 - probs )
#   }
# }


#' @param q a vector of quantiles
#' @param lower.tail a logical indicating whether to use the lower or upper tail probability
#' @return For \code{pdata}, a vector of probabilities
#' @rdname pqrdata
#' @examples
#' data(iris)
#' pdata(3:6, iris$Sepal.Length)
#' pdata(3:6, Sepal.Length, data=iris)
#' pdata(3:6, ~Sepal.Length, data=iris)
#'
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

#' @rdname pqrdata
pdata_f <- aggregatingFunction1( pdata_v, output.multiple=TRUE, na.rm=TRUE )

#' @rdname pqrdata
pdata <- function (q, vals, data = NULL, ...) 
{
  vals_call <- substitute(vals)
  args <- eval(substitute(alist(vals_call, ...)))
  args[["q"]] <- q
  args[["data"]] <- data
  do.call("pdata_f", args)
}

# #'
# #' @rdname pqrdata
# .pdata_old = function(q, vals, data=NULL, lower.tail=TRUE, ... ) {
#    if( !is.null(data) ) {
#          vals = eval( substitute(vals), data, enclos=parent.frame())
#    }
#   .check_for_quant_input(vals)
# #  L = length(q)
# #  res = rep(0,L)
#   n <- sum( ! is.na(vals) )
#   probs <- sapply( q, function(q) { sum( vals <= q , na.rm=TRUE ) } ) / n
#   if (lower.tail) { 
#   	return(probs)
#   } else {
# 	return( 1 - probs )
#   }
# }


#' @rdname pqrdata
#' 
#' @param n number of values to sample
#' @param replace  a logical indicating whether to sample with replacement
#' @return For \code{rdata}, a vector of values sampled from \code{vals} 
#' @rdname pqrdata'
#' @examples
#' data(iris)
#' rdata(10,iris$Species)
#' rdata(10, Species, data=iris)
#' rdata(10, ~Species, data=iris)
#' rdata(5, Sepal.Length~Species, data=iris)
#'

rdata_v <- function(vals, n, replace=TRUE, ... ) {
  sample( vals, n, replace=replace, ...)
}

#' @rdname pqrdata
rdata_f <- aggregatingFunction1( rdata_v, output.multiple=TRUE, na.rm=TRUE )

#' @rdname pqrdata
rdata <- function (n, vals, data = NULL, ...) 
{
  vals_call <- substitute(vals)
  args <- eval(substitute(alist(vals_call, ...)))
  args[["n"]] <- n
  args[["data"]] <- data
  do.call("rdata_f", args)
}

#rdata <- function(n, vals, data=NULL, replace=TRUE, ... ) {
#   if( !is.null(data) ) {
#         vals = eval( substitute(vals), data, enclos=parent.frame())
#   }
#  sample( vals, n, replace=replace, ...)
#}

#' @param v a vector of quantiles
#' @param log  a logical indicating whether the result should be log transformed
#' @return For \code{ddata}, a vector of probabilities (empirical densities)
#' @rdname pqrdata
#' @examples
#' data(iris)
#' ddata('setosa', iris$Species)
#' ddata('setosa', Species, data=iris)
#' ddata('setosa', ~Species, data=iris)

ddata_v <- function(vals, v, ..., data=NULL, log=FALSE, na.rm=TRUE) {
        if( !is.null(data) ) {
         vals = eval( substitute(vals), data, enclos=parent.frame())
        }
	n <- sum( ! is.na(vals) )
	probs <- sapply(v, function(x) { sum( vals == x, na.rm=na.rm) / n } )
	if (log) { probs <- log(probs) }
	return (probs)
}

#' @rdname pqrdata
ddata_f <- aggregatingFunction1( ddata_v, output.multiple=TRUE, na.rm=TRUE )

#' @rdname pqrdata
ddata <- function (v, vals, data = NULL, ...) 
{
  vals_call <- substitute(vals)
  args <- eval(substitute(alist(vals_call, ...)))
  args[["v"]] <- v
  args[["data"]] <- data
  do.call("ddata_f", args)
}

