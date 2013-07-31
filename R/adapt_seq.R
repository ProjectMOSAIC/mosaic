#' Adaptively generate sequences in an interval
#'
#' \code{adapt_seq} is similar to \code{seq} except that instead of 
#' selecting points equally spaced along an interval, it selects points
#' such that the values of a function applied at those points are 
#' (very) roughly equally spaced.  This can be useful for sampling 
#' a function in such a way that it can be plotted more smoothly, 
#' for example.
#'
#'
#' @param from start of interval
#' @param to end of interval
#' @param length.out desired length of sequence
#' @param f a function
#' @param args arguments passed to \code{f}
#' @return a numerical vector
#' @export
#' @examples
#' adapt_seq(0, pi, 25, sin)
#'
adapt_seq <-function(from, to, 
	length.out=100, 
	f=function(x,...){ 1 }, 
	args=list()
	) 
{
	n <- round(log(length.out))
	s <- seq(from, to, length.out=n)
	while (length(s) < length.out) {
		# subdivide all intervals
		ds <- diff(s)
		mid.s <- s[-1] - .5 * ds
		s <- sort( c(s, mid.s) )

		# subdivide again if function changing rapidly
		ds <- diff(s)
		mid.s <- s[-1] - .5 * ds
		y <- do.call(f, args=c(list(s), args))
		dy <- abs(diff(y))
		o <- rev(order(dy))
		m <- base::sum( dy > quantile(dy,.75, na.rm=TRUE), na.rm=TRUE )
		new.s <- mid.s[ o[ 1:m ] ]
		s <- sort( c(s, new.s) )
	}
	return(s)
}
