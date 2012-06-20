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
#' The inputs to the function are all variables appearing on either the left 
#' or right sides of the formula.  Those appearing in the right side will 
#' occur in the order specified.  Those not appearing in the right side will
#' appear in an unspecified order.
#' 
#' @examples
#' f <- makeFun( sin(x^2 * b) ~ x & y & a); f
#' g <- makeFun( sin(x^2 * b) ~ x & y & a, a=2 ); g
#' h <- makeFun( a * sin(x^2 * b) ~ b & y, a=2, y=3); h

setGeneric(
		   "makeFun",
		   function( object, ... )
		   {
			   standardGeneric('makeFun')
		   }
		   )

#' @rdname makeFun
#' @aliases makeFun,formula-method
#' @param strict.declaration  if \code{TRUE} (the default), an error is thrown if 
#' default values are given for variables not appearing in the \code{object} formula.

setMethod(
  'makeFun',
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

#' @rdname makeFun
#' @aliases makeFun,lm-method
#' @examples
#' model <- lm(wage ~ poly(exper,degree=2), data=CPS)
#' fit <- makeFun(model)
#' xyplot(wage ~ exper, data=CPS)
#' plotFun(fit(exper) ~ exper, add=TRUE)

setMethod(
  'makeFun',
  'lm',
   function( object, ... ) {
	  vars <- model.vars(object)
	  result <- function(){}
	  if ( length( vars ) <  1 ) {
		  result <- function( ... ) {
			  dots <- list(...)
			  if (length(dots) > 0) {
				  x <- dots[[1]] 
				  dots[[1]] <- NULL
			  } else {
				  x <- 1
			  }
			  do.call(predict, c(list(model, newdata=data.frame(x=x)), dots))
		  }
	  } else {
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
	  }

	  # myenv <- parent.frame()
	  # myenv$model <- object
	  # environment(result) <- myenv
	  environment(result) <- list2env( list(model=object) )
	  attr(result,"coefficients") <- coef(object)
	  return(result)
  }
  )

#' @rdname makeFun
#' @aliases makeFun,glm-method
#' @examples
#' model <- glm(wage ~ poly(exper,degree=2), data=CPS, family=gaussian)
#' fit <- makeFun(model)
#' xyplot(wage ~ exper, data=CPS)
#' plotFun(fit(exper) ~ exper, add=TRUE)

setMethod(
  'makeFun',
  'glm',
   function( object, ..., type='response' ) {
	  vars <- model.vars(object)
	  result <- function(){}
	  if ( length( vars ) <  1 ) {
		  result <- function( ... ) {
			  dots <- list(...)
			  if (length(dots) > 0) {
				  x <- dots[[1]] 
				  dots[[1]] <- NULL
			  } else {
				  x <- 1
			  }
			  do.call(predict, c(list(model, newdata=data.frame(x=x)), dots))
		  }
	  } else {
		  body(result) <- 
			  parse( text=paste(
								"return(predict(model, newdata=data.frame(",
								paste(vars, "= ", vars, collapse=",", sep=""), 
								"), ..., type=type))"
								)
		  )
		  formals(result) <- 
			  eval(parse( 
						 text=paste( "as.pairlist(alist(", 
									paste(vars, "= ",  collapse=",", sep=""), ", ...=))"
		  )
		  ))
	  }

	  # myenv <- parent.frame()
	  # myenv$model <- object
	  # environment(result) <- myenv
	  environment(result) <- list2env( list(model=object) )
	  attr(result,"coefficients") <- coef(object)
	  return(result)
  }
  )

#' @rdname makeFun
#' @aliases makeFun,nls-method
#' @examples
#' model <- nls( wage ~ A + B * exper + C * exper^2, data=CPS, start=list(A=1,B=1,C=1) )
#' fit <- makeFun(model)
#' xyplot(wage ~ exper, data=CPS)
#' plotFun(fit(exper) ~ exper, add=TRUE)

setMethod(
  'makeFun',
  'nls',
   function( object, ... ) {
	  vars <- setdiff(all.vars(rhs(object$m$formula())), names(coef(object)))
	  result <- function(){}
	  if ( length( vars ) <  1 ) {
		  result <- function( ... ) {
			  dots <- list(...)
			  if (length(dots) > 0) {
				  x <- dots[[1]] 
				  dots[[1]] <- NULL
			  } else {
				  x <- 1
			  }
			  do.call(predict, c(list(model, newdata=data.frame(x=x)), dots))
		  }
	  } else {
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
	  }

	  # myenv <- parent.frame()
	  # myenv$model <- object
	  # environment(result) <- myenv
	  environment(result) <- list2env( list(model=object) )
	  attr(result,"coefficients") <- coef(object)
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

#' Extract coefficients from a function
#'
#' \code{coef}  will extract the coefficients attribute from a function.
#' Functions created by applying \code{link{makeFun}} to a model produced
#' by \code{\link{lm}}, \code{\link{glm}}, or \code{\link{nls}} store
#' the model coefficients there to enable this extraction.
#' 
#' @name coef
#' @rdname coef
#' @aliases coef coef.function 
#'
#' @param object a function
#' @param ... ignored
#'
#' @export
#' @method coef function
#' @examples
#' model <- lm( width ~ length, data=KidsFeet)
#' f <- makeFun( model )
#' coef(f)
#' coefficients(f)

coef.function <- function(object,...) { attr(object,"coefficients") }

# @name coef
# @rdname coef
# @aliases coefficients coefficients.function
# @method coefficients function
# @export
# coefficients.function <- function(object,...) { attr(object,"coefficients") }
