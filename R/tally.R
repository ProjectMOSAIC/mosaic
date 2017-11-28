#' Turn logicals into factors; leave other things alone
#' 
#' Turn logicals into factors; leave other things alone
#'
#' @param x a vector or data frame
#' @param \dots additional arguments (currently ignored)
#' @return If `x` is a vector either `x` or the result
#' of converting `x` into a factor with levels `TRUE`
#' and `FALSE` (in that order);  if `x` is a data frame,
#' a data frame with all logicals converted to factors in this manner.

#' @rdname logical2factor
#' @export

logical2factor  <- function(x, ...) { UseMethod('logical2factor') }

#' @rdname logical2factor
#' @export
 
logical2factor.default  <- function( x, ... ) {
	if (is.logical(x)) {
		x <- factor(x, levels=c(TRUE,FALSE), labels=c("TRUE","FALSE"))
	}
	return(x)
}

#' @rdname logical2factor
#' @export

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
#' @param data a data frame or environment in which evaluation occurs.
#' Note that the default is `data=parent.frame()`.  This makes it convenient to
#' use this function interactively by treating the working envionment as if it were 
#' a data frame.  But this may not be appropriate for programming uses.  
#' When programming, it is best to use an explicit `data` argument
#' -- ideally supplying a data frame that contains the variables mentioned
#' @param format a character string describing the desired format of the results.
#'        One of `'default'`, `'count'`, `'proportion'`, `'percent'`, 
#'        `'data.frame'`, `'sparse'`, or `'default'`.
#'        In case of `'default'`, counts are used unless there is a condition, in
#'        which case proportions are used instead.  Note that prior to version 0.9.3, 
#'        `'default'` was the default, now it is `'count'`.
#'        `'data.frame'` converts the table to a data frame with one row per cell;
#'        `'sparse'` additionally removes any rows with 0 counts.
#'        
#' @param subset an expression evaluating to a logical vector used to select a subset of `data`
#' @param quiet a logical indicating whether messages about order in which 
#'   marginal distributions are calculated should be suppressed.  
#'   See [addmargins()].
#' @param groups used to specify a condition as an alternative to using a formula
#' with a condition.
#' @param margins a logical indicating whether marginal distributions should be displayed.
#' @param useNA as in [table()], but the default here is `"ifany"`.
#' @param envir an environment in which to evaluate
#' @param groups.first a logical indicating whether groups should be inserted 
#' ahead of the condition (else after).
#' @param ... additional arguments passed to [table()]
#' @return A object of class `"table"`, unless passing through to \pkg{dplyr}
#'  or converted to a data frame because `format` is `"data.frame"` or 
#'  `"sparse"`.
#' @details
#' The \pkg{dplyr} package also exports a [dplyr::tally()] function.  
#' If `x` inherits from class `"tbl"` or `"data frame"`, 
#' then \pkg{dplyr}'s [dplyr::tally()] is called.  This makes it
#' easier to have the two packages coexist.
#' 
#' Otherwise, `tally()` is designed as an alternative to [table()] and 
#' [xtabs()].  The primary use case it to describe a (possibly multi-dimensional)
#' table using a formula.  For a table of counts, each component of the formula becomes one 
#' of the dimensions of the cross table.  For tables of proportions or percents, conditional
#' proportions and percents are computed, conditioned on each level of all "secondary" 
#' (i.e., conditioning) variables, defined as everything other than the left hand side, 
#' if there is a left hand side to the formula; and everything except the right hand side
#' if the left hand side of the formula is empty.  Note that `groups` is folded into
#' the formula prior to this determination and becomes part of the conditioning.
#' 
#' When marginal totals are added, they are added for all of the conditioning dimensions, and 
#' proportions should sum to 1 for each level of the conditioning variables.  This can be 
#' useful to make it clear which conditional proportions are being computed.
#' 
#' See the examples for some typical use cases.
#' 
#' @note The current implementation when `format = "sparse"` first creates the full data frame
#' and then removes the unneeded rows.  So the savings is in terms of space, not time.
#' @examples
#' tally( ~ substance, data = HELPrct)
#' tally( ~ substance + sex , data = HELPrct)
#' tally( sex ~ substance, data = HELPrct)   # equivalent to tally( ~ sex | substance, ... )
#' tally( ~ substance | sex , data = HELPrct)
#' tally( ~ substance | sex , data = HELPrct, format = 'count', margins = TRUE)
#' tally( ~ substance + sex , data = HELPrct, format = 'percent', margins = TRUE)
#' tally( ~ substance | sex , data = HELPrct, format = 'percent', margins = TRUE)
#' # force NAs to show up
#' tally( ~ sex, data = HELPrct, useNA = "always")
#' # show NAs if any are there
#' tally( ~ link, data = HELPrct)
#' # ignore the NAs
#' tally( ~ link, data = HELPrct, useNA = "no")
#' @export

tally <- function(x, ...) {
  UseMethod("tally")
}

#' @rdname tally
#' @param wt for weighted tallying, 
#'   see [dplyr::tally()] in \pkg{dplyr}
#' @param sort a logical, 
#'   see [dplyr::tally()] in \pkg{dplyr}

#' @export
tally.tbl <- function(x, wt, sort=FALSE, ..., envir=parent.frame()) {
  if (missing(wt)) {
    return(do.call(dplyr::tally, list(x, sort=sort), envir=envir))
  } else {
    return(do.call(dplyr::tally, list(x, wt=substitute(wt), sort=sort), envir=envir))
  }
}

#' @rdname tally

#' @export
tally.data.frame <- function(x, wt, sort=FALSE, ..., envir=parent.frame()) {
  if (missing(wt)) {
    return(do.call(dplyr::tally, list(x, sort=sort), envir=envir))
  } else {
    return(do.call(dplyr::tally, list(x, wt=substitute(wt), sort=sort), envir=envir))
  }
}

#' @rdname tally
#' @export
tally.formula <- 
  function(x, data = parent.frame(2), 
           format=c('count', 'proportion', 'percent', 'data.frame', 'sparse', 'default'), 
           margins=FALSE,
           quiet=TRUE,
           subset, 
           groups = NULL,
           useNA = "ifany", 
           groups.first = FALSE,
           ...) {
 	format <- match.arg(format)
 	formula_orig <- x
	formula <- mosaic_formula_q(x, groups = groups, max.slots = 3, groups.first = groups.first)
	evalF <- evalFormula(formula, data)

	if (!missing(subset)) {
		subset <- eval(substitute(subset), data, environment(formula))
		if (!is.null(evalF$left))           evalF$left <- evalF$left[subset, , drop=FALSE]
		if (!is.null(evalF$right))         evalF$right <- evalF$right[subset, , drop=FALSE]
		if (!is.null(evalF$condition)) evalF$condition <- evalF$condition[subset, , drop=FALSE]
	}

	if (format == 'default'){ # exists mainly for historical reasons, not actually the default
		if (is.null(evalF$condition) ) format <- 'count'
		else format <- 'proportion'
	}

	res <- table(logical2factor( joinFrames(evalF$left, evalF$right, evalF$condition) ), 
	             useNA = useNA, ...)

	if (any(names(dimnames(res)) == "")) {
	  names(dimnames(res)) <- c(names(evalF$left), names(evalF$right), names(evalF$condition))
	}
	
	dims <- seq_along(dim(res))
	if (!is.null(evalF$condition) && ncol(evalF$condition) > 0) {
	  prop_columns <- tail(dims, ncol(evalF$condition))
	} else {
	  if (is.null(evalF$left)) {
	    prop_columns <- numeric(0)
	  } else {
	    prop_columns <- tail(dims, ncol(evalF$right))
	  }
	}
	
	res <- switch(format,
		   'count' =  res,
       'data.frame' = as.data.frame(res),
       'sparse' = {res <- as.data.frame(res); res <- res[res$Freq > 0,]},
		   'proportion' = 
		   		prop.table(res, margin = prop_columns),
		   'percent' = 
		   		100 * prop.table(res, margin = prop_columns)
		   )
	if (margins & ! format %in% c("data.frame", "sparse")) {
	  # default: add margins for the non-condition dimensions of the table
	  # but there are some exceptions when everything is marginal
	  margin_columns <- head(dims, -length(prop_columns))
	  if (length(margin_columns) == 0L)  margin_columns <- dims
	  res <-  addmargins(res, margin_columns, FUN = list(Total = sum), quiet = quiet)
	}
	return(res)
}

#' @export
tally.default <- 
  function(x, format=c('count', 'proportion', 'percent', 'data.frame', 'sparse', 'default'), 
           margins=FALSE,
           quiet=TRUE,
           subset, 
           useNA = "ifany", 
           data = parent.frame(2),
           ...) {
    D <- data_frame(X = x)
    tally( 
      ~ X, data = D, format = format, margins = margins, 
      quiet = quiet, subset = subset, useNA = useNA,
      ...)
  }
    
  
#' return a vector of row or column indices
#'
#' return a vector of row or column indices
#' 
#' @param x an object that may or may not have any rows or columns
#' @param default what to return if there are no rows or columns
#' @return if `x` has rows or columns, a vector of indices, else `default`
#' @rdname columns
#' @examples
#' columns(iris)
#' dim(HELPrct)
#' columns(HELPrct)
#' rows(HELPrct)
#' columns(NULL)
#' columns("this doesn't have columns")
#' @export

columns <- function(x, default=c()) {
	hi <- ncol(x)
	if (is.null(hi)) return(default) else  return( 1:hi )
}

#' @rdname columns
#' @export

rows <- function(x, default=c()) {
	hi <- nrow(x)
	if (is.null(hi) || hi < 1) return(default) else  return( 1:hi )
}

#' Compute proportions, percents, or counts for a single level
#' 
#' Compute proportions, percents, or counts for a single level
#'
#' @rdname prop
#' @param x an R object, usually a formula
#' @param data a data frame in which `x` is to be evaluated
#' @param \dots arguments passed through to [tally()]
#' @param success the level for which counts, proportions or percents are 
#'         calculated
#' @param level Deprecated.  Use `sucess`.
#' @param long.names a logical indicating whether long names should be 
#'         when there is a conditioning variable
#' @param sep a character used to separate portions of long names
#' @param useNA an indication of how NA's should be handled.  By default, they are
#'   ignored.
#' @param format one of `proportion`, `percent`, or `count`,
#'        possibly abbreviated
#' @param pval.adjust a logical indicating whether the "p-value" adjustment should be 
#' applied.  This adjustment adds 1 to the numerator and denominator counts.
#' @param quiet a logical indicating whether messages regarding the 
#'   success level should be supressed.
#'
#' @note For 0-1 data, `success` is set to 1 by default since that a standard 
#' coding scheme for success and failure.
#' 
#' @details
#' `prop1` is intended for the computation of p-values from randomization
#' distributions and differs from `prop` only in its default value of 
#' `pval.adjust`.
#' 
#' @examples
#' prop( ~sex, data=HELPrct)
#' prop( ~sex, data=HELPrct, success = "male")
#' count( ~sex | substance, data=HELPrct)
#' prop( ~sex | substance, data=HELPrct)
#' perc( ~sex | substance, data=HELPrct)
#' @export

prop <- function(x, data=parent.frame(), useNA = "no", ..., 
                 success = NULL, 
                 level = NULL, 
                 long.names=TRUE, sep=".", 
                 format=c("proportion", "percent", "count"), 
                 quiet=TRUE,
                 pval.adjust = FALSE) {
  format <- match.arg(format)
  if (!is.null(level)) stop("`level' has been depricated.  Use `success' instead.")
  
  T <- mosaic::tally(x, data=data, useNA = useNA, ...)
  
  scale <- if (format == "percent") 100.0 else 1.0
  
  if (length(dim(T)) < 1) stop("Insufficient dimensions.")
  lnames <- dimnames(T)[[1]]
  if (is.null(success)) {
	  success <- lnames[1]
  	if (success == 'FALSE') success <- 'TRUE'
    if (success == 0 && lnames[2] ==1 && length(lnames) == 2) {
      success <- 1
    }
  }
  if (! success %in% lnames) stop(
    paste("I don't see any such level.  Only", paste(lnames, collapse=", "))
    )
  if (! quiet) 
    message(paste0( "    success: ", success, 
                    ";  other levels: ", 
                    paste(setdiff(lnames, success), collapse=", "), "\n" ) )
  if ( length(dim(T)) == 2) {
    idx <- match(success, lnames)
    if (format == "count")
      result <- T[idx,] 
    else
      result <- ((T[idx,] + pval.adjust) / (colSums(T) + pval.adjust)) * scale
    if (long.names)
      names(result) <- paste(success, names(result), sep=sep)
    return(result)
  }
  if ( length(dim(T)) == 1) {
    idx <- match(success, names(T))
    result <- if (format == "count") 
      T[idx] 
    else
      (T[idx] + pval.adjust) / (sum(T) + pval.adjust) * scale
    return(result)
  }
  stop(paste('Too many dimensions (', length(dim(T)), ")",sep=""))
}

#' @rdname prop
#' @export
prop1 <- function(..., pval.adjust = TRUE) {
  prop(..., pval.adjust = pval.adjust)
}

#' @rdname prop
#' @export

count <- function(x, ...) {
  UseMethod("count")
}

#' @export
count.data.frame <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)
  dplyr::count_(x, vars, wt, sort = sort)
}

#' @export
count.tbl <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)
  dplyr::count_(x, vars, wt, sort = sort)
}

#' @export
count.default <- function(x, data = parent.frame(), ..., format="count") {
	prop(x, data = data, ..., format = format)
}

# count_ <- function(x, data=parent.frame(), ..., format="count") {
#	prop(x, data=data, ..., format=format)
# }

#' @rdname prop
#' @export

perc <- function(x, data=parent.frame(), ..., format="percent") {
	prop(x, data=data, ..., format=format)
}
