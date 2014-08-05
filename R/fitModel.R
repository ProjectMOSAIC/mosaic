#'
#' Fit a nonlinear least squares model
#'
#' Allows you to specify a formula with parameters, along with starting
#' guesses for the parameters.  Refines those guesses to find the 
#' least-squares fit.
#'
#' @return a function
#' 
#' @param formula formula specifying the model
#' @param data dataframe containing the data to be used
#' @param start passed as \code{start} to \code{\link{nls}}.  If and empty list,
#' a simple starting point is used (thus avoiding the usual warning message).
#' @param \dots additional arguments passed to \code{\link{nls}}
#' @param object an R object (typically a the result of fitModel)
#' 
#' @details
#' Fits a nonlinear least squares model to data.  In contrast
#' to linear models, all the parameters (including linear ones)
#' need to be named in the formula.  The function returned 
#' simply contains the formula together with pre-assigned 
#' arguments setting the parameter value.  Variables used in the 
#' fitting (as opposed to parameters) are unassigned arguments 
#' to the returned function.  
#' @note
#' This doesn't work with categorical explanatory variables.
#'
#' @seealso \code{\link{linearModel}}, \code{\link{nls}}
#'
#' @examples
#' f <- fitModel(temp ~ A+B*exp(-k*time), data=CoolingWater, start=list(A=50,B=50,k=1/20))
#' f(time=50)
#' coef(f)
#' summary(f)
#' model(f)
#' @export

fitModel <- function(formula, data=parent.frame(), start=list(), ...) {
  argsAndParams <- all.vars(rhs(formula))    # [-1]
  response <- eval(lhs(formula), data)
  n <- length(response)
  for ( nm in setdiff(argsAndParams, names(start)) ) {
  	  x <- tryCatch(get( nm, data), error=function(e) {list()} )  
	  if (length (x)  != n && nm != "pi") start[[nm]] <- 1
  }

  model <- nls(formula, data=data, start=start, ... )
  result <- makeFun(model)
  class(result) <- c("nlsfunction", class(result))
  return(result)
}

#' @rdname fitModel
#' @export
model <- function(object, ...) {
  UseMethod('model')
}

#' @rdname fitModel
#' @export
model.nlsfunction <- function(object, ...) {
  as.list(environment(object))$model
}

#' @rdname fitModel
#' @export
setMethod("summary", "nlsfunction",
		  function(object, ...) {
			  summary( model( object), ... )
		  }
)

#' @rdname fitModel
#' @export
coef.nlsfunction <- function(object, ...) {
  coef( model(object), ... )
}
