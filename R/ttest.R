#' T-tests
#' 
#' This wrapper around \code{t.test} in the \code{stats} package adds the ability to describe
#' one-sample t via a formula.
#' 
#' @param x an object
#' @param ... additional arguments, generally passed directly to \code{stats::t.test}. One 
#' notable exception is the argument \code{data}, which specifies a data frame in which to
#' interpret a formala \code{x} in the case of one-sample t.
#' @export
#' @examples
#' t.test( ~ age, data=HELPrct )
#' 
t.test <- function(x, ...) {
  tryCatch( return(stats::t.test(x, ...)), 
            error=function(e) {})
  if ( ! .is.formula(x) ) 
    stop( "Try using a formula as the first argument." )
  
  formula <- x 
  dots <- list(...)
  if ( is.null( dots[["data"]] ) ) stop ("You must specify a data frame with data=")

  evalF <- evalFormula(formula,dots[['data']])
  if (ncol(evalF$right) < 1L) 
    stop("No data specified in rhs of formula.") 
  
  vname <- names(evalF$right)[1L]
  if (ncol(evalF$right) > 1L) {  
    warning(paste("Multiple variables specified in rhs of formula.  I'm only using ", vname, ".", sep=""))
  }
  
  dataName <- paste("data$",vname,sep="")
  x <- evalF$right[,1]
  dots[['data']] <- NULL
  result <- do.call( stats::t.test, c(list(x=x), dots) ) 
  result$data.name <- dataName
  return(result)

}