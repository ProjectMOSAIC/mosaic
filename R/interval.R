#' Extract summary statistics
#' 
#' Extract confidence intervals, test statistics or p-values from an 
#' \code{htest} object.
#' @param x An object of class \code{htest}.
#' @param \dots Additional arguments.
#' @return the extracted p-value, confidence interval, or test statistic
#' 
#' 
#' @export
#' @examples
#' confint(t.test(rnorm(100)))
#' pval(t.test(rnorm(100)))
#' stat(t.test(rnorm(100)))
#' confint(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))
#' pval(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))
#' 
#' data(HELPrct)
#' stat(t.test (age ~ shuffle(sex), HELPrct))
#' # Compare to test statistic computed with permuted values of sex.
#' do(10) * stat(t.test (age ~ shuffle(sex), HELPrct))
#' 
#' @keywords stats 
#' @keywords inference 
#' 

#' @rdname interval
#' @param object a fitted model object or an htest object.
#' @param parm a specification of which parameters are to be given confidence intervals, 
#' either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.

interval <- confint

# interval <- (x, ...){UseMethod("interval", x)}
#
#' @rdname interval
#' @method confint htest
#' @param verbose a logical

confint.htest <- function (object, verbose=FALSE, ...){
  int <- object$conf.int
  lev <- attr(int, "conf.level")
  if (verbose ) {
	  message( "\n" )
	  message('Method: ')
	  message(object$method)
	  message( "\n" )
	  message( "\n" )
	  print(object$estimate) 
	  message( "\n" )
	  message( paste(lev * 100, "% confidence interval: \n", sep = "") )
	  message( as.vector(int) )
	  message( "\n" )
	  message( "\n" )
  	  invisible(int)
  }
  interv <- as.vector(int) 
  names(interv) <- c('lower','upper')
  level <- c(level=lev)
  int <- c(object$estimate, interv, level )
  return(int)
}

#' @rdname interval
#' @keywords stats 
#' @keywords inference 
pval <- function(x, ...){UseMethod("pval", x)}

#' @rdname interval
#' @method pval htest
#' @param digits number of digits to display in verbose output

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
#' @keywords stats 
#' @keywords inference 
stat <- function(x,...) { UseMethod("stat", x)}

#' @rdname interval
#' @method stat htest

stat.htest <- function(x,...) {
	x $ statistic
}
