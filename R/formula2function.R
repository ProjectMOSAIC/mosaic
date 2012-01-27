#' Create a function from a formula
#' 
#' Provides an easy mechanism for creating simple "mathematical" 
#' functions via a formula interface.
#' 
#' @param .formula a formula describing the function.  This should generally
#'         be specified without naming.
#' @param \dots additional arguments in the form \code{var = val} that
#' set default values for the inputs to the function.
#' @return a function
#' 
#' @details
#' The definition of the function is given by the left side of a formula.  The right
#' side lists at least one of the inputs to the function.
#' The inptus to the function are all variables appearing on either the left 
#' or right sides of the formula.  Those appearing in the right side will 
#' occur in the order specified.  Those not appearing in the right side will
#' appear in an unspecified order.
#' 
#' @examples
#' f <- formula2function( sin(x^2 * b) ~ x & y & a); f
#' g <- formula2function( sin(x^2 * b) ~ x & y & a, a=2, y=3); g
#' h <- formula2function( sin(x^2 * b) ~ b & y, a=2, y=3); h

formula2function <- function( .formula, ... ) {
  sexpr <- .formula 
  if (! inherits( sexpr, "formula") || length(sexpr) != 3) 
	  stop('First argument must be a formula with both left and right sides.')

  vals <- list(...)
  expr <- eval(sexpr)  # not sure if eval() is needed here or not.
  lhs <- expr[[2]]
  rhs <- expr[[3]]
  
  rhsVars <- all.vars(rhs)
  lhsOnlyVars <- setdiff(all.vars(expr), rhsVars)
  vars <- c(rhsVars, lhsOnlyVars)
  
  valVec <- rep("", length(vars))
  names(valVec) <- vars
  for( n in names(vals) ) valVec[n] <- as.character(vals[n]) 
  
  result <- function(){}
  body(result) <- parse( text=deparse(lhs) ) 
  formals(result) <- eval(parse(
    text=paste( "alist(", paste(vars, "=", valVec, collapse=",", sep=""), ")")
    ))
  return(result)  
}
