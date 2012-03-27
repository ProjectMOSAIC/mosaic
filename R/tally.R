
#' Turn logicals into factors; leave other things alone
#'
#' @param x a vector or data frame
#' @param \dots additional arguments (currently ignored)
#' @return If \code{x} is a vector either \code{x} or the result
#' of converting \code{x} into a factor with levels \code{TRUE}
#' and \code{FALSE} (in that order);  if \code{x} is a data frame,
#' a data frame with all logicals converted to factors in this manner.
#'
#' @rdname logical2factor

logical2factor  <- function(x, ...) { UseMethod('logical2factor') }

#' @rdname logical2factor
#' @method logical2factor default
logical2factor.default  <- function( x, ... ) {
	if (is.logical(x)) {
		x <- factor(x, levels=c(TRUE,FALSE), labels=c("TRUE","FALSE"))
	}
	return(x)
}

#' @rdname logical2factor
#' @method logical2factor data.frame
logical2factor.data.frame  <- function( x, ... ) {
	for (var in names(x)) {
		if (is.logical(x[,var])) {
			x[,var] <- logical2factor(x[,var])
		}
	}
	return(x)
}


#' Tabulate categorical data
#'
#' @param formula a formula describing the type of table desired
#' @param data a data frame or environment in which evaluation occurs
#' @param format a character string describing the desired format of the results.
#'        One of \code{'default'}, \code{'count'}, \code{'proportion'}, or \code{'percent'}.
#'        In case of \code{'default'}, counts are used unless there is a condition, in
#'        which case proportions are used instead.
#' @param subset an expression evaluating to a logical vector used to select a subset of \code{data}
#' @param quiet a logical indicating whether messages about order in which marginal distributions
#'        are calculated should be surpressed.  See \code{\link{addmargins}}.
#' @param margins a logical indicating whether marginal distributions should be displayed.
#' @export
#' @examples
#' tally( ~ substance, HELPrct)
#' tally( ~ substance & sex , HELPrct)
#' tally( sex ~ substance, HELPrct)   # equivalent to tally( ~ sex | substance, ... )
#' tally( ~ substance | sex , HELPrct)
#' tally( ~ substance | sex , HELPrct, format='count')
#' tally( ~ substance & sex , HELPrct, format='percent')

setGeneric( 
	"tally", 
	function(x, ... )  {
		standardGeneric('tally')
	}
)

#' @rdname tally
#' @aliases tally,ANY-method
#' @usage tally(x, ...)

setMethod(
	'tally',
	'ANY',
    function(x, ...) {
		dd <- data.frame(x=x)
		tally(~ x, dd, ...)
	}
)

#' @rdname tally
#' @aliases tally,formula-method
#' @param formula a formula
#'
#' @param data a data frame
#'
#' @param format one of \code{default}, \code{count}, \code{proportion}, or \code{percent} describing
#'        the format the tallies should be returned in.
#'
#' @param margins a logical indicating whether margins tallies should be added.
#'
#' @param quiet a logical indicating whether tallying should be done quietly (vs. verbosely)
#'
#' @param subset an expression defining a subset of the data frame to be tallied.
#'
#' @export

setMethod(
	'tally',
	'formula',
    function(x, data=parent.frame(), 
				   format=c('default','count','proportion','percent'), 
				   margins=TRUE,
				   quiet=TRUE,
				   subset, ...) {
	format <- match.arg(format)
	formula <- x
	evalF <- evalFormula(formula,data)

	if (!missing(subset)) {
		subset <- eval(substitute(subset), data, environment(formula))
		if (!is.null(evalF$left))           evalF$left <- evalF$left[subset,]
		if (!is.null(evalF$right))         evalF$right <- evalF$right[subset,]
		if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset,]
	}

	# shift things around if lhs exists and condition is empty
	if (!is.null (evalF$left) && is.null(evalF$condition)) {
		evalF$condition <- evalF$right
		evalF$right <- evalF$left
		evalF$left <- NULL
	}

	if (format == 'default'){
		if (is.null(evalF$condition) ) format <- 'count'
		else format <- 'proportion'
	}

	res <- table( logical2factor( joinFrames(evalF$right,evalF$condition) ) )

	res <- switch(format,
		   'count' = 
				res,
		   'proportion' = 
		   		prop.table( res, margin = ncol(evalF$right) + columns(evalF$condition) ),
		   'percent' = 
		   		100 * prop.table( res, margin = ncol(evalF$right) + columns(evalF$condition) )
		   )
	if (margins) {  # add margins for the non-condition dimensions of the table
		res <- addmargins(res, 1:ncol(evalF$right), FUN=list(Total=sum), quiet=quiet )
	}
	return(res)
}
)

#' return a vector of row or column indices
#'
#' @param x an object that may or may not have any rows or columns
#' @param default what to return if there are no rows or columns
#' @return if \code{x} has rows or columns, a vector of indices, else \code{default}
#' @rdname columns
#' @examples
#' dim(HELPrct)
#' columns(HELPrct)
#' rows(HELPrct)
#' columns(NULL)
#' columns("this doesn't have columns")

columns <- function(x, default=c()) {
	hi <- ncol(x)
	if (is.null(hi)) return(default) else  return( 1:hi )
}

#' @rdname columns
rows <- function(x, default=c()) {
	hi <- nrow(x)
	if (is.null(hi)) return(default) else  return( 1:hi )
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
#' evalFormula(wage ~ sex & married & age | sector & race, data=cps)

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
    res <- data.frame(eval(x, envir=data))
    names(res) <- deparse(x)
    return( res )
  }
  else return(joinFrames( evalSubFormula(x[[2]],data), evalSubFormula(x[[3]],data)))
}

#' Join data frames
#'
#' @param left,right data frames
#' @param \dots data frames to be joined
#' @rdname joinFrames
#' @return a data frame containing columns from each of data frames being joined.
#' @export

joinFrames <- function(...) {
	dots <- list(...)
	if (length(dots) == 0) return(NULL)
	if (length(dots) == 1) return(dots[[1]])
	if (length(dots) == 2) return(joinTwoFrames(dots[[1]],dots[[2]]))
	first <- dots[[1]]; dots[[1]] <- NULL
	return( joinTwoFrames( first, do.call(joinFrames, dots)) )
} 

#' @rdname joinFrames
joinTwoFrames <- function(left, right){
    if( is.null(right)) return(left)
    if( is.null(left)) return(right)
    # this is to keep names like "cross(sex,hair)" intact
    result <-  data.frame(left, right)
    names(result) <- c((names(left)),(names(right)))
    return(result)
} 

#' Compute proportions, percents, or counts for a single level
#'
#' @rdname prop
#' @param \dots arguments passed through to \code{\link{tally}}
#' @param level the level for which counts, proportions or percents are 
#'         calculated
#' @param long.names a logical indicating whether long names should be 
#'         when there is a conditioning variable
#' @param sep a character used to separate portions of long names
#' @param format one of \code{proportion}, \code{percent}, or \code{count},
#'        possibly abbrevaited
#' @export
#' @examples
#' prop( ~sex, data=HELPrct)
#' prop( ~sex, data=HELPrct, level='male')
#' count( ~sex | substance, data=HELPrct)
#' prop( ~sex | substance, data=HELPrct)
#' perc( ~sex | substance, data=HELPrct)

prop <- function(...,level=NULL, long.names=TRUE, sep=":", format="proportion") {
  T <- tally(..., format=format)
  if (length(dim(T)) < 1) stop("Insufficient dimensions.")
  if (is.null(level)) {
	  level <- dimnames(T)[[1]][1]
  	  if (level == 'FALSE') level <- 'TRUE'
  }
  if ( length(dim(T)) == 2) {
    result <- T[level,]
    if (long.names)
      names(result) <- paste(level, names(result), sep=sep)
    return(result)
  }
  if ( length(dim(T)) == 1) {
    result <- T[level]
    return(result)
  }
  stop(paste('Too many dimensions (', length(dim(T)), ")",sep=""))
}

#' @rdname prop
count <- function(..., format="count") {
	prop(..., format=format)
}

#' @rdname prop
perc <- function(..., format="percent") {
	prop(..., format=format)
}
