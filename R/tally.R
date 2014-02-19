
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
logical2factor.default  <- function( x, ... ) {
	if (is.logical(x)) {
		x <- factor(x, levels=c(TRUE,FALSE), labels=c("TRUE","FALSE"))
	}
	return(x)
}

#' @rdname logical2factor
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
#' Tabulate categorical data
#'
#' @rdname tally
#' @aliases tally
#'
#' @param x an object
#' @param data a data frame or environment in which evaluation occurs
#' @param format a character string describing the desired format of the results.
#'        One of \code{'default'}, \code{'count'}, \code{'proportion'}, or \code{'percent'}.
#'        In case of \code{'default'}, counts are used unless there is a condition, in
#'        which case proportions are used instead.
#' @param subset an expression evaluating to a logical vector used to select a subset of \code{data}
#' @param quiet a logical indicating whether messages about order in which marginal distributions
#'        are calculated should be surpressed.  See \code{\link{addmargins}}.
#' @param margins a logical indicating whether marginal distributions should be displayed.
#' @param ... additional arguments passed to \code{\link{table}}
#' @export
#' @examples
#' tally( ~ substance, data=HELPrct)
#' tally( ~ substance & sex , data=HELPrct)
#' tally( sex ~ substance, data=HELPrct)   # equivalent to tally( ~ sex | substance, ... )
#' tally( ~ substance | sex , data=HELPrct)
#' tally( ~ substance | sex , data=HELPrct, format='count')
#' tally( ~ substance & sex , data=HELPrct, format='percent')
#' tally( ~ link, data=HELPrct, useNA="always")


tally <- function(x, data=parent.frame(), 
                      format=c('default','count','proportion','percent'), 
                      margins=FALSE,
                      quiet=TRUE,
                      subset, ...) {
	format <- match.arg(format)
  if (! .is.formula(x) ) {
      formula <- ~ x
      formula[[2]] <- substitute(x)
      message( "First argument should be a formula... But I'll try to guess what you meant")
      return(
        do.call(tally, list(formula, data=data, format=format, margins=margins, quiet=quiet, ...))
      )  
  }
  
	formula <- x
	evalF <- evalFormula(formula,data)

	if (!missing(subset)) {
		subset <- eval(substitute(subset), data, environment(formula))
		if (!is.null(evalF$left))           evalF$left <- evalF$left[subset, , drop=FALSE]
		if (!is.null(evalF$right))         evalF$right <- evalF$right[subset, , drop=FALSE]
		if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset, , drop=FALSE]
	}
  
  # provide warning for 3-slot formulas
  
	if (!is.null (evalF$left) && ! is.null(evalF$condition)) {
    stop( "Unsupported formula type." )
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

	res <- table( logical2factor( joinFrames(evalF$right,evalF$condition) ), ... )

	res <- switch(format,
		   'count' = 
				res,
		   'proportion' = 
		   		prop.table( res, margin = ncol(evalF$right) + columns(evalF$condition) ),
		   'percent' = 
		   		100 * prop.table( res, margin = ncol(evalF$right) + columns(evalF$condition) )
		   )
	if (margins) {  # add margins for the non-condition dimensions of the table
    if ( !is.null(evalF$right) & ncol(evalF$right) > 0 )
		  res <- addmargins(res, 1:ncol(evalF$right), FUN=list(Total=sum), quiet=quiet )
	}
	return(res)
}

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
	if (is.null(hi) || hi < 1) return(default) else  return( 1:hi )
}

#' Compute proportions, percents, or counts for a single level
#'
#' @rdname prop
#' @param x an R object, usually a formula
#' @param data a data frame in which \code{x} is to be evaluated
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

prop <- function(x, data=parent.frame(), ..., level=NULL, long.names=TRUE, sep=".", format="proportion") {
  T <- tally(x, data=data, ..., format=format)
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
count <- function(x, data=parent.frame(), ..., format="count") {
	prop(x, data=data, ..., format=format)
}

#' @rdname prop
perc <- function(x, data=parent.frame(), ..., format="percent") {
	prop(x, data=data, ..., format=format)
}
