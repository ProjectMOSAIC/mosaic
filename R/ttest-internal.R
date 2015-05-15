### internally use ttest method.

ttest <- function (x, ..., data=parent.frame()) {
  UseMethod('ttest', x) 
}

ttest.default <-  function (x, y = NULL, ..., data=parent.frame()) {
  stats::t.test(x, y, ...)
}

ttest.formula <- function(x, y, ..., groups=NULL, data=parent.frame()) {
  x <- mosaic_formula_q(x, groups=groups, max.slots=2, 
                        envir = if (is.environment(data)) data else environment(x))
  # if (is.null(x)) stop("Invalid formula specification.")
  tryCatch( 
    return(stats::t.test(x, data=data, ...)),
    error=function(e) {
      if (grepl("grouping factor must have exactly 2 levels", e$message)) stop(e)
    })
  dots <- list(...)
  formula <- x
  
  evalF <- evalFormula(formula,data)
  if (ncol(evalF$right) < 1L) 
    stop("No data specified in rhs of formula.") 
  
  vname <- names(evalF$right)[1L]
  if (ncol(evalF$right) > 1L) {  
    stop("Multiple variables specified in rhs of formula.")
  }
  
  dataName <- paste("data$",vname,sep="")
  x <- evalF$right[,1]
  result <- do.call( stats::t.test, c(list(x=x), dots) ) 
  result$data.name <- dataName
  return(result)
}
