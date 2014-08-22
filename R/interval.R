#' Extract summary statistics
#' 
#' Extract confidence intervals, test statistics or p-values from an 
#'   \code{htest} object.
#' @rdname interval
#' @param object a fitted model object or an htest object.
#' @param parm a specification of which parameters are to be given confidence intervals, 
#'   either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#' @param x An object of class \code{htest}.
#' @param \dots Additional arguments.
#' @return the extracted p-value, confidence interval, or test statistic
#' 
#' @examples
#' confint(t.test(rnorm(100)))
#' pval(t.test(rnorm(100)))
#' stat(t.test(rnorm(100)))
#' confint(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))
#' pval(var.test(rnorm(10,sd=1), rnorm(20, sd=2)))
#' if (require(mosaicData)) {
#' data(HELPrct)
#' stat(t.test (age ~ shuffle(sex), HELPrct))
#' # Compare to test statistic computed with permuted values of sex.
#' do(10) * stat(t.test (age ~ shuffle(sex), HELPrct))
#' }
#' @keywords stats 
#' @keywords inference 
#' @export

interval <- confint

# interval <- (x, ...){UseMethod("interval", x)}
#
#' @rdname interval
#' @param verbose a logical
#' @export

confint.htest <- function (object, parm, level, ...){
  if (! missing( parm ) || !missing( level ) ) { warning("parm and level are ignored.") }
  int <- object$conf.int
  lev <- attr(int, "conf.level")
  verbose <- list(...)[['verbose']]
  if (is.null(verbose)) verbose <- FALSE
  if ( verbose ) {
	  cat('\nMethod: ')
	  cat(object$method)
	  cat('\nEstimate: ')
	  cat( format(object$estimate, getOption('digits',3)) )
	  cat( paste("\n", lev * 100, "% confidence interval: ", sep = "") )
	  cat( paste(format(as.vector(int), getOption('digits',3)), collapse=" ") )
  	  return(invisible(int))
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
#' @export

pval <- function(x, ...){UseMethod("pval", x)}

#' @rdname interval
#' @param digits number of digits to display in verbose output
#' @export

pval.htest <- function (x, digits=4, verbose=FALSE, ...){
  pval <- x$p.value
  if (!verbose) {
    return( c(p.value=pval) )
  }
  # verbose stuff below
  stat <- x$statistic
  param <- x$parameter
  method <- x$method
  cat('\n')
  cat( paste('Method: ', method, "\n", sep="") )
  
  tryCatch( {
    alt <- x$alternative
    direction <- switch(alt, 
                        'less' = ' < ',
                        'greater' = ' > ',
                        'two.sided' = ' <> ',
                        ' <> '
    )
    null <- x$null.value
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
    estimate <- x$estimate
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
  }, error=function(e) {}
  )
  cat( paste("p-value = ", signif(pval,digits), sep="") ) 
  cat('\n\n')
  return(invisible(c(p.value=pval)))
}

#' @rdname interval
#' @keywords stats 
#' @keywords inference 
#' @export

stat <- function(x,...) { UseMethod("stat", x)}

#' @rdname interval
#' @export

stat.htest <- function(x,...) {
	x $ statistic
}

#' Extract r-squared value
#' 
#' Attempts to extract an r-squared value from a model or model-like object.
#' @param x an object
#' @param \dots additional arguments
#' @export

rsquared <- function(x, ...) {
  NULLFUN <- function(e) NULL 
  result <- tryCatch( x$r.squared, error=NULLFUN)
  if (is.null(result)) 
    result <- tryCatch( summary(x, ...)$r.squared, error=NULLFUN )
  return(result)
}

#' Extract r-squared value
#' 
#' Attempts to extract an r-squared value from a model or model-like object.
#' @param x an object
#' @param \dots additional arguments
#' @export

r.squared <- function(x, ...) {
  .Deprecated("rsquared")
  rsquared(x, ...)
}
