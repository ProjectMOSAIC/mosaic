#' Tabulate categorical data
#'
#' @param formula a formula describing the type of table desired
#' @param data a data frame or environment in which evaluation occurs
#' @param proportion a logical indicating whether the results should be returned as proportions
#' rahter than counts
#' @param margins a logical indication whether marginal distributions should be displayed.
#' @export
#' @examples
#' mtable( ~ substance, HELPrct)
#' mtable( ~ substance & sex , HELPrct)
#' mtable( sex ~ substance, HELPrct)
#' mtable( ~ substance | sex , HELPrct)
#' mtable( ~ substance | sex , HELPrct, proportion=TRUE)
#' mtable( ~ substance & sex , HELPrct, proportion=TRUE)

mtable <- function(formula, data=parent.frame(), proportion=FALSE, margins=TRUE) {
	evalF <- evalFormula(formula,data)
	if (is.null(evalF$condition)) {
		evalF$condition <- evalF$right
		evalF$right <- evalF$left
		evalF$left <- NULL
	}
	res <- table(joinFrames(evalF$right,evalF$condition))
	if (proportion)
		res <- prop.table(res, margin = ncol(evalF$right) + (1:ncol(evalF$condition)) )
	if (margins)
		return(addmargins(res))
	else 
		return(res)
}

#' Evaluate a formula 
#' 
#' @param formula a formula (\code{ y ~ x | z}) to evaluate
#' @param data a data frame or environment in which evaluation occurs
#' @return a list containing data frames corresponding to the left, right, and condition
#' slots of \code{formula}
#' @export
#' @examples
#' data(CPS)
#' cps <- CPS[1:6,]
#' cps
#' evalFormula( wage ~ sex & married & age | sector & race , data=cps)

evalFormula <- function(formula, data=parent.frame()) {
	# could make this an S4 object instead
	return( list(
				 left      = evalSubFormula(      lhs(formula), data), 
				 right     = evalSubFormula(      rhs(formula), data), 
				 condition = evalSubFormula(condition(formula), data) 
				 ) )
}

# evalSubFormula and evalFormula could be made methods with a common generic

#' Evaluate a part of a formula
#'
#' @param x an object appearing as a subformula (typically a call)
#' @param data a data fram or environment in which things are evaluated
#' @param split a vector of operators that are not evaluated as operators but
#'      instead used to further split \code{x}
#' @return a data frame containing the terms of the evaluated subformula
#' @export
#' @examples
#' data(CPS)
#' cps <- CPS[1:6,]
#' cps
#' evalSubFormula( rhs( ~ married & sector), data=cps )

evalSubFormula <- function(x, data=parent.frame(), split=c('&') ){
  if (is.null(x)) return(NULL)
  if( is.name(x) || !(as.character(x[[1]]) %in% split) ) {
    res <- data.frame(eval(x, env=data))
    names(res) <- deparse(x)
    return( res )
  }
  else return(joinFrames( evalSubFormula(x[[2]],data), evalSubFormula(x[[3]],data)))
}

#' Join data frames
#'
#' @param left,right data frames
#' @return a data frame containing columns from each of \code{left} and \code{right}
#' @export

joinFrames <- function(left, right){
    if( is.null(right)) return(left)
    if( is.null(left)) return(right)
    # this is to keep names like "cross(sex,hair)" intact
    result <-  data.frame(left, right)
    names(result) <- c((names(left)),(names(right)))
    return(result)
} 
