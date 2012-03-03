#' Symbolic Derivatives
#'
#' Constructs symbolic derivatives of some mathematical expressions
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @rdname symbolicD
#' @name symbolicD
#' @aliases symbolicD
#'
#' @param formula a mathematical expression (see examples and \code{\link{plotFun}})
#' @param \dots additional parameters, typically default values for mathematical parameters
#'
#' @return a function implementing the derivative 
#'
#' @details
#' Uses the built-in symbolic differentiation function to construct 
#' a formula for the derivative and packages this up as a function.
#'
#' @seealso \code{\link{D}}, \code{\link{symbolicD}}, \code{\link{makeFunction}}, \code{\link{antiD}}, \code{\link{plotFun}}
#'
#' @examples
#' symbolicD( a*x^2 ~ x)
#' symbolicD( a*x^2 ~ x&x)
#' symbolicD( a*sin(x)~x, .order=4)
#' symbolicD( a*x^2*y+b*y ~ x, a=10, b=100 )
symbolicD <- function(formula, ..., .order=NULL ) {
  formulaEnv = environment(formula) # where was the formula created?
  # Find the expression and the variables "with respect to" which differentiation 
  # is to be taken: "wrt"
  pF <- parse.formula(formula)
  df <- lhs(pF)
  # get the list of w.r.t. variables
  wrtNames <- all.vars(rhs(pF), unique=FALSE)
  # handle a derivative whose order is specified by a number
  if( !is.null(.order)) {
    if( .order < 0 | .order != round(.order)) 
      stop("Derivative of negative or fractional order not available.")
    else if(.order==0) return(makeFunction(formula, ...))
    # Set up list appropriately for order of the derivative.
    wrtNames <- rep(wrtNames[1], round(.order))
  }
  # Create a holder for the output function.
  #res = makeFunction( formula, ... )
  # Basing the holder on the original "formula" 
  # will allow formal parameters which disappear 
  # in the derivative creation process
  for (j in 1:length(wrtNames)) {
    df <- stats::D(df,wrtNames[j])
  }
  # plug in df as the new function
  
  # This didn't seem to work: strict.declaration doesn't have the desired effect
#  formula[[2]] <- df
#   res = makeFunction( formula, ..., strict.declaration=FALSE)
  # Instead
  res = makeFunction( formula, ... )
  body(res) <- df
  
  
  # the function should come from the same place as the formula
  environment(res) = formulaEnv
  return(res)
}