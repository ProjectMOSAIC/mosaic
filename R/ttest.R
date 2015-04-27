#' Student's t-Test
#' 
#' Performs one and two sample t-tests.  
#' The mosaic \code{t.test} provides wrapper functions around the function 
#' of the same name in \pkg{stats}.
#' These wrappers provide an extended interface that allows for a more systematic
#' use of the formula interface.
#' 
#' @rdname ttest
#' 
#' @param x an object (e.g., a formula or a numeric vector)
#' @param data a data frame
#' @param groups \code{x = ~ var, groups=g} is equivalent to \code{ x = var ~ g }.
#' 
#'
#' @param \dots  additional arguments, see \code{\link[stats]{t.test}} in the
#'    \pkg{stats} package.
#' 
#' @return an object of class \code{htest}
#' 
#' @details
#' This is a wrapper around \code{\link{t.test}} from the \pkg{stats} package
#' to extend the functionality of the formula interface.
#'
#' @seealso \code{\link[mosaic]{prop.test}}, \code{\link[stats]{t.test}}
#' 
#' @examples
#' if (require(mosaicData)) {
#' t.test( ~ age, data=HELPrct)
#' t.test( age ~ sex, data=HELPrct)
#' t.test( ~ age | sex, data=HELPrct)
#' t.test( ~ age, groups=sex, data=HELPrct)
#' }
#' @export t.test
  
t.test <- function(x, ...) ttest(x, ...)

#' rdname ttest
#' @export

ttest <- function (x, ...) {
  UseMethod('ttest') 
}

#' @rdname ttest
#' @export

ttest.default <-  function (x, ...) {
  stats::t.test(x = x, ...)
}

#' @rdname ttest
#' @export

ttest.formula <- function(x, data=parent.frame(), groups=NULL, ...) {
  x <- mosaic_formula_q(x, groups=groups, max.slots=2)
  # if (is.null(x)) stop("Invalid formula specification.")
  tryCatch( 
    return(stats::t.test(x, data=data, ...)),
    error=function(e) {
      if (grepl("grouping factor must have exactly 2 levels", e$message)) stop(e)
      })
  dots <- list(...)
  formula <- x

#  if (is.null(data)) stop("data must be specified.")

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
