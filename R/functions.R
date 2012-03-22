#' Utilities for creating mathematical functions from expressions
#'
#' \code{makeFun} turns a mathematical expression (written as a formula)
#' into the corresponding R function.  It's purely a convenience that uses the
#' same expression syntax as \code{plotFun}, \code{D} and \code{antiD}.
#'
#' @name makeFun
#'
#' @param sexpr the mathematical expression written as a formula
#'
#' @param ... additional arguments setting default numerical values
#' for arguments or parameters
#'
#' @return a function
#' @export
#'
#' @examples
#' f = makeFun(sin(x^2) ~ x)
#' g = makeFun(a*x + b ~ x,a=2,b=4)
#' g(x=10)
#' g(x=10,a=5,b=2)
#' h = makeFun(sin(x)*y ~ x&y)
#'
makeFun = function( sexpr=NULL, ... ) {
  foo = .createMathFun( sexpr=sexpr, ... )   # find this
  x = foo$fun
#  attr(x,"mosaicType") = "Constructed function"
#  attr(x,"functionExpression") = foo$sexpr
  body(x) = foo$sexpr
  return(x)
}

#' @rdname printfunction
#' @method print function
#' @title Printing Calculus Functions
#' @name print.function
#'
#' @param x the function to be printed
#' @param default use the standard function printing methods
#' @param useSource passed along to the standard function printing methods
#' @param \dots additional arguments 
#'
#' @return nothing
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#'
#' @details
#' The mosaic calculus operators such as
#' \code{D} and \code{antiD} and \code{linearModel}
#' sometimes create functions with an obscure numerical method as the
#' internal contents.
#' This print method detects such functions (through their \code{kind} attribute) and
#' provides a more friendly display
#' @examples
#' F <- antiD(sin(x^2)~x)
#' F
#' print(F)
#' print(F, default=TRUE)
#'

print.function = function(x, useSource=TRUE, default=FALSE, ...) {
  kind = attr(x,"mosaicType")
  if( is.null(kind) | default) {
    7
  }
  else {
    if( kind %in% c("Numerical finite-difference process","Numerical integration process")) {
      hoo = formals(x)
      if ("..args" %in% names(hoo)) {
        goo = hoo$..args
        hoo$..args = NULL        
      }
      formals(x) = hoo
      body(x) = paste(kind,"on", deparse(goo$sexpr), "with respect to", goo$names  )
    }
    else if( kind == "Fitted Linear Model") {
      vals = x(showcoefs=TRUE) # get the coefficients
      nms = names(vals)
      cc = as.numeric(vals)
      if( nms[1] == "(Intercept)" ) nms[1] = "1"
      body(x) = parse(text=paste(cc, nms, sep="*",collapse="+"))
    }
  }
  base::print.function(x, useSource=useSource, ...)
}
