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
#' @param ... named initial values for parameters
#' @param options not yet used
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
#' stan <- fetchData("stan-data.csv")
#' f <- nlsModel(temp ~ A+B*exp(-k*time), data=stan,A=50,B=50,k=1/20)
#' f(time=50)
#'
nlsModel <- function(formula, data, ..., options) {
  input.names <- all.vars(formula)[-1]
  L <- nls(formula,data=data, start=list(...))
  params = as.list(L$m$getPars())
  all.the.inputs = all.vars( formula[[3]])
  just.the.arguments = setdiff( all.the.inputs, names(params) )
  F <- function() {
    if( showcoefs ) coef(L)
    else { # evaluate the function 
      D <- eval(parse(text=makeDF))
      L$m$predict(newdata=D)
    }
  }
  tmp <- paste("alist( ", paste(just.the.arguments, "=", collapse = ",", sep = ""),")")
  tmp <- eval(parse(text = tmp))
  tmp <- c(tmp,params)
  formals(F) <- tmp
  body(F) <- formula[[3]]
  attr(F,"mosaicType") <- "Fitted Nonlinear Least Squares Model"
  return(F)
}
