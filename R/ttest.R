
#' Student's t-Test
#' 
#' Performs one and two sample t-tests.  
#' The mosaic \code{t.test} provides wrapper functions around the function 
#' of the same name in \pkg{stats}.
#' These wrappers provide an extended interface that allows for a more systematic
#' use of the formula interface.
#' 
#' @rdname t.test
#'
#' @param formula a formula describing the test to be conducted.
#' 
#' @param data a data frame (if missing, \code{y} may be a data frame)
#'           a data frame if \code{x} is a formula
#'
#' @param subset an optional vector specifying a subset of observations to be used.  
#' Currently ignored.
#'
#' @param na.action a function which indicates what should happen when the data 
#' contain \code{NA}s.
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
#' @export
#' @examples
#' t.test( ~ age, data=HELPrct)
#' t.test( age ~ sex, data=HELPrct)
#' t.test( ~ age | sex, data=HELPrct)
#' t.test( ~ mother + father, Galton)
#' 
#' @keywords stats
#' @method t.test formula

t.test.formula <- function (formula, data=parent.frame(), subset, na.action, ...) 
{
    if (missing(formula))
        stop("'formula' missing or incorrect")

    evalF <- evalFormula( formula, data )
    df <- joinFrames( evalF$left, evalF$right, evalF$condition ) 

    if (ncol(df) < 1) stop("'formula' missing or incorrect")

    if (ncol(df) == 1) {
		x <- evalF$right[,1]
		y <- NULL
		result <- stats:::t.test.default(x, y, ...) 
		result$data.name <- paste(deparse(substitute(data)), "$", names(evalF$right)[1], sep="")
		return(result)
    }

    if ( ncol(df) >=2 && is.numeric(df[,2]) )  {
		result <- stats:::t.test.default(df[,1], df[,2], ...) 
		result$data.name <- sub("df[, 1]", paste(deparse(substitute(data)),"$df[, 1]",sep=""), 
				result$data.name, fixed=TRUE) 
		result$data.name <- sub("df[, 2]", paste(deparse(substitute(data)),"$df[, 2]",sep=""), 
				result$data.name, fixed=TRUE) 
		result$data.name <- sub("df[, 2]", names(df)[2], result$data.name, fixed=TRUE) 
		result$data.name <- sub("df[, 1]", names(df)[1], result$data.name, fixed=TRUE) 
		result$data.name <- sub("df[, 2]", names(df)[2], result$data.name, fixed=TRUE) 
		return(result)
    }

    if ( ncol(df) >=2 && ! is.numeric(df[,2]) )  {
		sdf <- split(df[,1], df[,2])
		if ( length(sdf) != 2 ) stop("'formula' missing or incorrect")
		result <- stats:::t.test.default(sdf[[1]], sdf[[2]], ...)
		result$data.name <- sub("sdf[[1]]", names(sdf)[1], result$data.name, fixed=TRUE) 
		result$data.name <- sub("sdf[[2]]", names(sdf)[2], result$data.name, fixed=TRUE) 
		return(result)
    }

    stop("'formula' missing or incorrect")
}

# #' @rdname t.test
# #' @usage t.test( x, ... )
# #' @export
# t.test <- stats:::t.test
