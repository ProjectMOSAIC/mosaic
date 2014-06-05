tryCatch(utils::globalVariables(c('model','result')), 
		 error=function(e) message('Looks like you should update R.'))
#' Create a function from a formula
#' 
#' Provides an easy mechanism for creating simple "mathematical" 
#' functions via a formula interface.
#' 
#' @param object an object from which to create a function.  This should generally
#'         be specified without naming.
#' @param ... additional arguments in the form \code{var = val} that
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
#' @export

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
#' @param use.environment if \code{TRUE}, then variables implicitly defined in the 
#' \code{object} formula can take default values from the environment at the time 
#' \code{makeFun} is called.  A warning message alerts the user to this situation, 
#' unless \code{suppress.warnings} is \code{TRUE}.
#' @param suppress.warnings A logical indicating whether warnings should be suppressed.
#' @param transform a function used to transform the response.
#' This can be useful to invert a transformation used on the response
#' when creating the model.
#' @examples
#' model <- lm( log(length) ~ log(width), data=KidsFeet)
#' f <- makeFun(model, transform=exp)
#' f(8.4)
#' head(KidsFeet,1)
#' 
#' @export

setMethod(
  'makeFun',
  'formula',
  function( object, ..., strict.declaration =TRUE, use.environment=TRUE, suppress.warnings=FALSE) {
	  sexpr <- object 
	  if (! inherits( sexpr, "formula") || length(sexpr) != 3) 
		  stop('First argument must be a formula with both left and right sides.')

	  dots <- list(...)
	  expr <- eval(sexpr)  # not sure if eval() is needed here or not.
	  lhs <- lhs(expr) # expr[[2]]
	  rhs <- rhs(expr)  # expr[[3]]

	  rhsVars <- all.vars(rhs)
	  lhsOnlyVars <- setdiff(all.vars(lhs), rhsVars)
	  lhsOnlyVars <- setdiff(lhsOnlyVars,'pi')    # don't treat pi like a variable
	  varsInFormula <- c(rhsVars, lhsOnlyVars)
    varsWithDefaults <- intersect( names(dots), varsInFormula )
    varsWithoutDefaults <- setdiff(varsInFormula, varsWithDefaults)
    varsFromEnv <- character(0)
	  # declaredVars <- union(varsInFormula, varsWithDefaults)  # unDeclaredVars)
	  undeclaredVars <- setdiff(names(dots), varsInFormula) 
	  if (length( undeclaredVars ) != 0) {
		  if (strict.declaration) 
		  	stop(paste( "Default values provided for variables not in formula:",
					   paste(unDeclaredVars, collapse=",")
					 ))
	  }
    # vars is just a permutation of varsInFormula
    vars <- c(varsWithoutDefaults, varsWithDefaults)
	  valVec <- rep("", length(vars))
	  names(valVec) <- vars
    
	  for( n in varsWithDefaults ) valVec[n] <- as.character(dots[[n]]) 
  
    if (use.environment) {
      for( n in setdiff(varsWithoutDefaults, rhsVars) ) {
        v <- tryCatch(get(n, parent.frame()), error=function(e) "")
        if (is.numeric(v)) {
          valVec[n] <- as.character(v)
          varsFromEnv <- c(varsFromEnv,n)
          varsWithoutDefaults <- setdiff(varsWithoutDefaults, n)
        }
      }
    }
    varsDangerous <- intersect(lhsOnlyVars, varsWithoutDefaults)
    varsWithoutDefaults <- setdiff(varsWithoutDefaults, varsDangerous)
    finalVars <- c(varsWithoutDefaults, varsWithDefaults, varsFromEnv, varsDangerous)
    # finalVars <- c(finalVars, setdiff(vars,finalVars))
    w <- which (valVec=="")
    if (length(varsFromEnv) > 0 & !suppress.warnings)  
      warning(paste("Some default values taken from current environment: ", 
                    paste(varsFromEnv, collapse=", ") ))
	  if (length(varsDangerous) > 0 & !suppress.warnings)  
	    warning(paste("Implicit variables without default values (dangerous!): ", 
	                  paste(varsDangerous, collapse=", ") ))

	  result <- function(){}
	  body(result) <- parse( text=deparse(lhs) ) 
	  formals(result) <- 
		 eval(parse( 
			text=paste( "as.pairlist(alist(", 
					paste(finalVars, "=", valVec[finalVars], collapse=",", sep=""), "))"
	  			)
	  ))
	  environment(result) <- environment(object) # parent.frame()
	  return(result)  
  }
)

#' @rdname makeFun
#' @aliases makeFun,lm-method
#' @examples
#' model <- lm(wage ~ poly(exper,degree=2), data=CPS85)
#' fit <- makeFun(model)
#' xyplot(wage ~ exper, data=CPS85)
#' plotFun(fit(exper) ~ exper, add=TRUE)
#' @export

setMethod(
  'makeFun',
  'lm',
   function( object, ... , transform=identity) {
    dnames <- names(eval(object$call$data, parent.frame(1)))
	  vars <- modelVars(object)
    if (! is.null(dnames) ) vars <- intersect(vars, dnames)
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
			  transform( do.call(predict, c(list(model, newdata=data.frame(x=x)), dots)) )
		  }
	  } else {
		  body(result) <- 
			  parse( text=paste(
								"return(transform(predict(model, newdata=data.frame(",
								paste(vars, "= ", vars, collapse=",", sep=""), 
								"), ...)))"
								)
		  )
		  args <- paste("alist(", paste(vars, "=", collapse=",", sep=""),")")
		  args <- eval(parse(text=args))
		  args['pi'] <- NULL
		  args <- c(args, alist('...'=), list(transform=substitute(transform)))
		  formals(result) <- args
	  }

	  # myenv <- parent.frame()
	  # myenv$model <- object
	  # environment(result) <- myenv
	  environment(result) <- list2env( list(model=object, transform=transform) )
	  attr(result,"coefficients") <- coef(object)
	  return(result)
  }
  )

#' @rdname makeFun
#' @aliases makeFun,glm-method
#' @param type one of \code{'response'} (default) or \code{'link'} specifying scale to be used
#' for value of function returned.
#' @examples
#' model <- glm(wage ~ poly(exper,degree=2), data=CPS85, family=gaussian)
#' fit <- makeFun(model)
#' xyplot(wage ~ exper, data=CPS85)
#' plotFun(fit(exper) ~ exper, add=TRUE)
#' @export

setMethod(
  'makeFun',
  'glm',
   function( object, ..., type=c('response','link'), transform=identity ) {
	  type <- match.arg(type)
	  vars <- modelVars(object)
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
			  transform( do.call(predict, c(list(model, newdata=data.frame(x=x)), dots)) )
		  }
	  } else {
		  if (type == "link") {
		    body(result) <- 
			  parse( text=paste(
								"return(transform(predict(model, newdata=data.frame(",
								paste(vars, "= ", vars, collapse=",", sep=""), 
								"), ..., type='link')))"
								) )
		  } else {
		    body(result) <- 
			  parse( text=paste(
								"return(transform(predict(model, newdata=data.frame(",
								paste(vars, "= ", vars, collapse=",", sep=""), 
								"), ..., type='response')))"
								) )
		  }
		  args <- paste("alist(", paste(vars, "=", collapse=",", sep=""),")")
		  args <- eval(parse(text=args))
		  args['pi'] <- NULL
		  args <- c(args, alist('...'=), list(transform=substitute(transform)))		  
		  formals(result) <- args
		  
	  }

	  # myenv <- parent.frame()
	  # myenv$model <- object
	  # environment(result) <- myenv
	  environment(result) <- list2env( list(model=object, transform=transform) )
	  attr(result,"coefficients") <- coef(object)
	  return(result)
  }
  )

#' @rdname makeFun
#' @aliases makeFun,nls-method
#' @examples
#' model <- nls( wage ~ A + B * exper + C * exper^2, data=CPS85, start=list(A=1,B=1,C=1) )
#' fit <- makeFun(model)
#' xyplot(wage ~ exper, data=CPS85)
#' plotFun(fit(exper) ~ exper, add=TRUE)
#' @export

setMethod(
  'makeFun',
  'nls',
  function( object, ... , transform=identity) {
    dnames <- names(eval(object$call$data, parent.frame(1)))
    cvars <- names(coef(object))
    vars <- all.vars(rhs(eval(object$m$formula())))
    vars <- setdiff(vars, cvars) 
    if (! is.null(dnames) ) vars <- intersect(vars, dnames)
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
        transform( do.call(predict, c(list(model, newdata=data.frame(x=x)), dots)) )
      }
    } else {
      body(result) <- 
        parse( text=paste(
          "return(transform(predict(model, newdata=data.frame(",
          paste(vars, "= ", vars, collapse=",", sep=""), 
          "), ...)))"
        )
        )
      # params <- as.list(coef(object))  
      args <- paste("alist(", paste(vars, "=", collapse=",", sep=""),")")
      args <- eval(parse(text=args))
      # args <- c(args,params)
      args['pi'] <- NULL
      args <- c(args, alist('...'=), list(transform=substitute(transform)))		  
      formals(result) <- args
    }

    environment(result) <- list2env( list(model=object, transform=transform) )
    attr(result,"coefficients") <- coef(object)
    return(result)
  }
)


#' extract predictor variables from a model
#' 
#' @param model a model, typically of class \code{lm} or \code{glm}
#' @return a vector of variable names
#' @examples
#' model <- lm( wage ~ poly(exper,degree=2), data=CPS85 )
#' modelVars(model)
#' @export

modelVars <- function(model) {
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
#' @name coef.function
#' @rdname coef
#' @aliases coef coef.function 
#'
#' @param object a function
#' @param ... ignored
#'
#' @examples
#' model <- lm( width ~ length, data=KidsFeet)
#' f <- makeFun( model )
#' coef(f)
#' @export

coef.function <- function(object,...) { attr(object,"coefficients") }

