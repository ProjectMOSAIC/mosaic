#' Extract summary statistics
#' 
#' Extract confidence intervals, test statistics or p-values from an 
#' \code{htest} object.
#' @param x An object of class \code{htest}.
#' @param \dots Additional arguments.
#' 
#' 
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#' @export
#' @examples
#' interval(t.test(rnorm(100)))
#' pval(t.test(rnorm(100)))
#' stat(t.test(rnorm(100)))
#' interval(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))
#' pval(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))
#' 
#' stat( t.test (age ~ shuffle(sex), HELP) )
#' # Compare to test statistic computed with permuted values of sex.
#' do(10) * stat( t.test (age ~ shuffle(sex), HELP) )
#' 
#' @keywords univar 
#' @keywords stats 
#' 

interval <- function(x, ...){UseMethod("interval", x)}

interval.htest <- function (x, verbose=FALSE, ...){
  int <- x$conf.int
  lev <- attr(int, "conf.level")
  if (verbose ) {
	  cat( "\n" )
	  cat('Method: ')
	  cat(x$method)
	  cat( "\n" )
	  cat( "\n" )
	  print(x$estimate) 
	  cat( "\n" )
	  cat( paste(lev * 100, "% confidence interval: \n", sep = "") )
	  cat( as.vector(int) )
	  cat( "\n" )
	  cat( "\n" )
  	  invisible(int)
  }
  interv <- as.vector(int) 
  names(interv) <- c('lower','upper')
  int <- c(x$estimate, interv )
  return(int)
}

#' @rdname interval
pval <- function(x, ...){UseMethod("pval", x)}

pval.htest <- function (x, digits=4, verbose=FALSE, ...){
  pval <- x$p.value
  stat <- x$statistic
  param <- x$parameter
  alt <- x$alternative
  method <- x$method
  null <- x$null.value
  estimate <- x$estimate
  direction <- switch(alt, 
  	'less' = ' < ',
  	'greater' = ' > ',
  	'two.sided' = ' <> '
	)
  if (verbose) {
	  cat('\n')
	  cat(paste('Method: ', method,  sep=""))
	  cat('\n\n')
	  cat(paste(
		'Null Hypothesis: ', 
		names(null), 
		" = ", 
		null,
		sep="") 
	  )  
	  cat('\n')
	  cat(paste(
		'Alt. Hypothesis: ', 
		names(null), 
		direction, 
		null,
		sep="") 
	  )  
	  cat('\n\n')
	  cat(paste(names(stat), " = ", 
		signif(stat,digits=digits),
		sep="") )  
	  cat('  (')
	  cat( paste( 
		names(param), " = ", 
		signif(param,digits=digits), 
		sep="",
		collapse=', ') )  
	  cat(')\n\n')
	  cat( paste("p-value = ", signif(pval,digits), sep="") ) 
	  cat('\n\n')
	  invisible(pval)
  }

  return( c(p.value=pval) )
}

#' @rdname interval
stat <- function(x,...) { UseMethod("stat", x)}

stat.htest <- function(x,...) {
	x $ statistic
}
