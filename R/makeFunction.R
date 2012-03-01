#' Create a function from a formula
#' 
#' Provides an easy mechanism for creating simple "mathematical" 
#' functions via a formula interface.
#' 
#' @param object an object from which to create a function.  This should generally
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
#' f <- makeFunction( sin(x^2 * b) ~ x & y & a); f
#' g <- makeFunction( sin(x^2 * b) ~ x & y & a, a=2 ); g
#' h <- makeFunction( a * sin(x^2 * b) ~ b & y, a=2, y=3); h

setGeneric(
		   "makeFunction",
		   function( object, ... )
		   {
			   standardGeneric('makeFunction')
		   }
		   )

#' @rdname makeFunction
#' @aliases makeFunction,formula-method
#' @param strict.declaration  if \code{TRUE} (the default), an error is thrown if 
#' default values are given for variables not appearing in the \code{object} formula.

setMethod(
  'makeFunction',
  'formula',
  function( object, ..., strict.declaration =TRUE ) {
	  sexpr <- object 
	  if (! inherits( sexpr, "formula") || length(sexpr) != 3) 
		  stop('First argument must be a formula with both left and right sides.')

	  vals <- list(...)
	  expr <- eval(sexpr)  # not sure if eval() is needed here or not.
	  lhs <- lhs(expr) # expr[[2]]
	  rhs <- rhs(expr)  # expr[[3]]

	  rhsVars <- all.vars(rhs)
	  lhsOnlyVars <- setdiff(all.vars(lhs), rhsVars)
	  lhsOnlyVars <- setdiff(lhsOnlyVars,'pi')    # don't treat pi like a variable
	  vars <- c(rhsVars, lhsOnlyVars)
	  unDeclaredVars <- setdiff(names(vals), vars) 
	  declaredVars <- setdiff(vars, unDeclaredVars)
	  if (length( unDeclaredVars ) != 0) {
		  if (strict.declaration) 
		  	stop(paste( "Default values provided for undeclared variables:",
					   paste(unDeclaredVars, collapse=",")
					 ))
		  vars <- declaredVars
	  }

	  valVec <- rep("", length(vars))
	  names(valVec) <- vars
	  for( n in intersect(vars, names(vals)) ) valVec[n] <- as.character(vals[n]) 

	  result <- function(){}
	  body(result) <- parse( text=deparse(lhs) ) 
	  formals(result) <- 
		 eval(parse( 
			text=paste( "as.pairlist(alist(", 
					paste(vars, "=", valVec, collapse=",", sep=""), "))"
	  			)
	  ))
	  environment(result) <- environment(object) # parent.frame()

	  return(result)  
  }
)

#' @rdname makeFunction
#' @aliases makeFunction,lm-method
#' @examples
#' model <- lm( wage ~ poly(exper,degree=2), data=CPS )
#' fit <- makeFunction(model)
#' xyplot( wage ~ exper, data=CPS)
#' plotFun(fit(exper) ~ exper, add=TRUE)

setMethod(
  'makeFunction',
  'lm',
   function( object, ... ) {
	  vars <- model.vars(object)
	  result <- function(){}
	  body(result) <- 
		  parse( text=paste(
						"return(predict(model, newdata=data.frame(",
						paste(vars, "= ", vars, collapse=",", sep=""), 
						"), ...))"
						)
	  	  )
	  formals(result) <- 
		  eval(parse( 
					 text=paste( "as.pairlist(alist(", 
								paste(vars, "= ",  collapse=",", sep=""), ", ...=))"
	  )
	  ))
	  # myenv <- parent.frame()
	  # myenv$model <- object
	  # environment(result) <- myenv
	  environment(result) <- list2env( list(model=object) )
	  return(result)
  }
  )

#' extract predictor variables from a model
#' 
#' @param model a model, typically of class \code{lm} or \code{glm}
#' @return a vector of variable names
#' @examples
#' model <- lm( wage ~ poly(exper,degree=2), data=CPS )
#' model.vars(model)
model.vars <- function(model) {
  formula <- as.formula(model$call$formula)
  all.vars(rhs(formula))
}
