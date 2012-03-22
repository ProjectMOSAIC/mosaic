#' Aggregating summary statistics
#' 
#' These drop-in replacements and new summary statistics functions are 
#' formula-aware and allow the use of simple names within data frames.  
#' When given formulas, they call \code{\link{aggregate}} using the
#' formula.
#' 
#' @details
#' These methods are wrappers around functions and methods in the \code{base} and \code{stats} 
#' packages and provide additional interfaces.
#' 
#' The default value for \code{na.rm} is reversed from the functions in \code{base} and \code{stats}.
#' Also, \code{na.rm}, \code{use}, and \code{trim} follow \code{\dots} so must be named using
#' their full names.
#'
#' @docType methods
#' @rdname aggregating-methods
#' @name aggregating-methods
#'
#'
#' @seealso 
#' \code{\link[stats]{aggregate}},
#' \code{\link[stats]{sd}},
#' \code{\link[stats]{var}},
#' \code{\link[stats]{median}},
#' \code{\link[base]{mean}},
#' \code{\link[base]{max}},
#' \code{\link[base]{min}},
#' \code{\link[base]{sum}}
#' 
#' @keywords methods 
#' @keywords stats 

##########################################################################################

#' @examples
#' data(HELPrct)
#' mean(age, data=HELPrct)
#' mean(~age, data=HELPrct)
#' mean(age ~ ., data=HELPrct)
#' mean(age ~ 1, data=HELPrct)
#' mean(age ~ NULL, data=HELPrct)
#' mean(HELPrct$age)
#' mean(age ~ sex, data=HELPrct)
#' mean(age ~ sex + treat, data=HELPrct)

#' @rdname aggregating-methods
#' @export
#' @usage mean(x, ..., na.rm=TRUE, trim=0)

setGeneric( 
	"mean", 
	function(x, ..., na.rm=TRUE, trim=0)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return( base::mean( eval(substitute(x), data), na.rm=na.rm, trim=trim) )
		}
		standardGeneric('mean')
	}
)

#' @rdname aggregating-methods
#' @aliases mean,ANY-method
#' @param x a vector
#' @param na.rm logical indicating whether NAs should be removed before calculating 
#' @param \dots additional arguments
# @param trim a numeric indicating the proportion to be trimmed from each tail before calculating mean
#' @export

setMethod(
	'mean',
	'ANY',
	function(x, ..., na.rm=TRUE, trim=0) 
		base::mean( .flatten(c(x,list(...))), na.rm=na.rm, trim=trim ) 
	
)

#' @rdname aggregating-methods
#' @aliases mean,numeric-method
#' @export

setMethod(
	'mean',
	'numeric',
	function(x, ..., na.rm=TRUE, trim=0) 
		base::mean( c(x,.flatten(list(...))), na.rm=na.rm, trim=trim ) 
	
)

#' @rdname aggregating-methods
#' @aliases mean,data.frame-method
#' @export
setMethod( 
	"mean", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE, trim=0) 
		base::mean(x=x, ..., na.rm=na.rm, trim=trim)
)

#' @rdname aggregating-methods
#' @aliases mean,formula-method
#' @export
setMethod( 
	"mean", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE, trim=0) {
		if( .is.simple.formula(x) ) {
			return( mean( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., na.rm=na.rm, trim=trim ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=base::mean, ..., na.rm=na.rm, trim=trim ) )
		} 
	}
)

##########################################################################################


#' @export
#' @docType methods
#' @rdname aggregating-methods
#'
#'
#' @examples
#' median(age, data=HELPrct)
#' median(~age, data=HELPrct)
#' median(age ~ ., data=HELPrct)
#' median(age ~ 1, data=HELPrct)
#' median(age ~ NULL, data=HELPrct)
#' median(HELPrct$age)
#' median(age ~ sex, data=HELPrct)
#' median(age ~ sex + treat, data=HELPrct)

setGeneric( 
	"median", 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(stats::median(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('median')
	}
)

#' @rdname aggregating-methods
#' @aliases median,ANY-method
#' @export
setMethod(
	'median',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		stats::median( .flatten(c(x,list(...))), na.rm=na.rm ) 
	
)

#' @rdname aggregating-methods
#' @aliases median,numeric-method
#' @export
setMethod(
	'median',
	'numeric',
	function(x, ..., na.rm=TRUE) 
		stats::median( c(x,.flatten(list(...))), na.rm=na.rm)
	
)

#' @rdname aggregating-methods
#' @aliases median,data.frame-method
#' @export
setMethod( 
	"median", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) sapply( x, stats::median, na.rm=na.rm)
)

#' @rdname aggregating-methods
#' @aliases median,formula-method
#' @export
setMethod( 
	"median", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( median( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::median, na.rm=na.rm) )
		} 
	}
)

##########################################################################################

#' @export
#' @docType methods
#' @rdname aggregating-methods
#'
#'
#' @examples
#' sd(age, data=HELPrct)
#' sd(~age, data=HELPrct)
#' sd(age ~ ., data=HELPrct)
#' sd(age ~ 1, data=HELPrct)
#' sd(age ~ NULL, data=HELPrct)
#' sd(HELPrct$age)
#' sd(age ~ sex, data=HELPrct)
#' sd(age ~ sex + treat, data=HELPrct)

setGeneric( 
	"sd", 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(stats::sd(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('sd')
	}
)

#' @rdname aggregating-methods
#' @aliases sd,ANY-method
#' @export
setMethod(
	'sd',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		stats::sd( .flatten(c(x,list(...))), na.rm=na.rm) 
)

#' @rdname aggregating-methods
#' @aliases sd,numeric-method
#' @export
setMethod(
	'sd',
	'numeric',
	function(x, ..., na.rm=TRUE) 
		stats::sd( c(x,.flatten(list(...))), na.rm=na.rm)
	
)

#' @rdname aggregating-methods
#' @aliases sd,data.frame-method
#' @export
setMethod( 
	"sd", 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) sapply( x, stats::sd, na.rm=na.rm)
)

#' @rdname aggregating-methods
#' @aliases sd,formula-method
#' @export
setMethod( 
	"sd", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( sd( eval( .simple.part(x), data, enclos=parent.frame()), ..., na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=SD, na.rm=na.rm) )
		} 
	}
)
##########################################################################################
# this is currently broken in that NAME does not get substituted as desired in standardGeneric()

.make.Maxlike.generic <- function( NAME=".Max", FUN = stats::max ) {

	setGeneric( 
		NAME, 
		function(x, ..., na.rm=TRUE)  {
			dots <- list(...)
			if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
				data <- dots[[1]]
				return(FUN(eval( substitute(x), data),  na.rm=na.rm))
			}
			standardGeneric(NAME)
		}
	)

	setMethod(
		NAME,
		'numeric',
		function(x, ..., na.rm=TRUE) {
			FUN(x, ..., na.rm=na.rm) 
		}
	)


	setMethod( 
		NAME, 
		signature=c("data.frame"),
		function(x, ..., na.rm=TRUE) {
			sapply( x, FUN, na.rm=na.rm)
		}
	)


	setMethod( 
		NAME, 
		signature=c("formula"),
		function(x, ..., na.rm=TRUE) {
			dots <- list(...)
			data  <- dots[[1]]

			if( .is.simple.formula(x) ) {
				return( FUN( eval( .simple.part(x), data, enclos=parent.frame()), na.rm=na.rm ) )
			} else {
				return( .mosaic_aggregate( x, data, FUN=FUN, na.rm=na.rm) )
			} 
		}
	)
}

#.make.Maxlike.generic( 'Max', base::max )
#.make.Maxlike.generic( 'Min', base::min )

###############################################################
# Because min and max are primatives, we need a work-around.  
# min and max are reassigned after creating .Min and .Max
# 
setGeneric( 
	'.Max', 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(base::max(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('.Max')
	}
)

setMethod(
	'.Max',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		base::max( x,..., na.rm=na.rm) 
)

setMethod(
	'.Max',
	'numeric',
	function(x, ..., na.rm=TRUE) {
		base::max(x, ..., na.rm=na.rm) 
	}
)


setMethod( 
	'.Max', 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) {
		sapply( x, base::max, na.rm=na.rm)
	}
)


setMethod( 
	'.Max', 
	signature=c("formula"),
	function(x, ..., na.rm=TRUE) {
		dots <- list(...)
		data  <- dots[[1]]

		if( .is.simple.formula(x) ) {
			return( base::max( eval( .simple.part(x), data, enclos=parent.frame()), na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=base::max, na.rm=na.rm) )
		} 
	}
)
###############################################################
setGeneric( 
	'.Min', 
	function(x, ..., na.rm=TRUE)  {
		dots <- list(...)
		if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(base::min(eval( substitute(x), data),  na.rm=na.rm))
		}
		standardGeneric('.Min')
	}
)

setMethod(
	'.Min',
	'ANY',
	function(x, ..., na.rm=TRUE) 
		base::min(x ,..., na.rm=na.rm)
)

setMethod(
	'.Min',
	'numeric',
	function(x, ..., na.rm=TRUE) {
		base::min(x, ..., na.rm=na.rm) 
	}
)


setMethod( 
	'.Min', 
	signature=c("data.frame"),
	function(x, ..., na.rm=TRUE) {
		sapply( x, base::min, na.rm=na.rm)
	}
)


setMethod( 
	'.Min', 
	signature=c("formula"),
	function(x, ..., na.rm=TRUE) {
		dots <- list(...)
		data  <- dots[[1]]

		if( .is.simple.formula(x) ) {
			return( base::min( eval( .simple.part(x), data, enclos=parent.frame()), na.rm=na.rm ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=base::min, na.rm=na.rm) )
		} 
	}
)

##########################################################################################



#' @export
#' @docType methods
#' @rdname aggregating-methods
# @param y second vector allows for computation of covariances
#'
#'
#' @examples
#' var(age, data=HELPrct)
#' var(~age, data=HELPrct)
#' var(age ~ ., data=HELPrct)
#' var(age ~ 1, data=HELPrct)
#' var(age ~ NULL, data=HELPrct)
#' var(HELPrct$age)
#' var(age ~ sex, data=HELPrct)
#' var(age ~ sex + treat, data=HELPrct)

setGeneric( 
	"var", 
	function(x, y=NULL, na.rm=TRUE, use, data=NULL)  {
		if ( is.name(substitute(x)) && ! is.null(y) && is.name(substitute(y)) && is.data.frame( data ) ) {
			return( stats::var(eval( substitute(x), data), eval(substitute(y, data), na.rm=na.rm, use=use)) )
		}
		if ( is.name(substitute(x)) && is.data.frame( y ) ) {
			return(stats::var(eval( substitute(x), y),  na.rm=na.rm))
		}
		if ( is.name(substitute(x)) && is.null(y) && is.data.frame( data ) ) {
			return( stats::var( eval( substitute(x), data), na.rm=na.rm, use=use) )
		}
		standardGeneric('var')
	}
)


#' @rdname aggregating-methods
#' @aliases var,ANY,ANY,ANY,ANY,ANY-method
#' @export
setMethod(
	'var',
	c('ANY','ANY'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		stats::var( x, y, na.rm=na.rm, use=use) 
)

#' @rdname aggregating-methods
#' @aliases var,numeric,numeric,ANY,ANY,ANY-method
#' @export
setMethod(
	'var',
	c('numeric','numeric'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		stats::var( x, y, na.rm=na.rm, use=use) 
)

#' @rdname aggregating-methods
#' @aliases var,numeric,ANY,ANY,ANY,ANY-method
#' @export
setMethod(
	'var',
	c('numeric'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		stats::var( x, y, na.rm=na.rm, use=use)
)

#' @rdname aggregating-methods
#' @aliases var,matrix,ANY,ANY,ANY,ANY-method
#' @export
setMethod(
	'var',
	c('matrix'),
	function(x, y, na.rm=TRUE, use=use, data=data) 
		stats::var( x, y, na.rm=na.rm, use=use) 
)

#' @rdname aggregating-methods
#' @aliases var,data.frame,ANY,ANY,ANY,ANY-method
#' @export
setMethod( 
	"var", 
	signature=c("data.frame"),
	function(x, y, na.rm=TRUE, use=use) stats::var(x, y, na.rm=na.rm, use=use)
)

#' @rdname aggregating-methods
#' @aliases var,formula,missing,ANY,ANY,data.frame-method
#' @export
setMethod( 
	"var", 
	signature=c(x="formula", y="missing", na.rm='ANY', use='ANY', data="data.frame"),
	function(x, na.rm=TRUE, use, data=parent.frame()) {
		if( .is.simple.formula(x) ) {
			return( stats::var( eval( .simple.part(x), data ),  na.rm=na.rm, use=use ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::var, na.rm=na.rm, use=use) )
		} 
	}
)

#' @rdname aggregating-methods
#' @aliases var,formula,data.frame,ANY,ANY,missing-method
#' @export
setMethod( 
	"var", 
	signature=c(x="formula", y="data.frame", na.rm='ANY', use='ANY', data="missing"),
	function(x, y=parent.frame(),  na.rm=TRUE, use) {
		data <- y
		if( .is.simple.formula(x) ) {
			return( stats::var( eval( .simple.part(x), data),  na.rm=na.rm, use=use ) )
		} else {
			return( .mosaic_aggregate( x, data, FUN=stats::var, na.rm=na.rm, use=use) )
		} 
	}
)
##########################################################################################

#' @rdname aggregating-methods
#' @examples
#' min(age, data=HELPrct)
min <- .Min

#' @rdname aggregating-methods
#' @examples
#' max(age, data=HELPrct)
#' max(~age, data=HELPrct)
#' max(age ~ ., data=HELPrct)
#' max(age ~ 1, data=HELPrct)
#' max(age ~ NULL, data=HELPrct)
#' max(HELPrct$age)
#' max(age ~ sex, data=HELPrct)
#' max(age ~ sex + treat, data=HELPrct)

max <- .Max

##########################################################################################
#' @export
#' @docType methods
#' @rdname aggregating-methods
# @param level  level for which the count or proportion is desired
#'
#' @examples
#' count(sex, data=HELPrct)
#' count(sex, data=HELPrct, level='male')
#' count(HELPrct$sex)

setGeneric('count',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		dots <- list(...)
			if ( ! .is.formula(x) && length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
				data <- dots[[1]]
				return( callGeneric(eval( substitute(x), data), level=level, na.rm=na.rm) ) 
			}
		standardGeneric('count')
	}
)

#' @rdname aggregating-methods
#' @aliases count,ANY-method
#' @export
setMethod(
	'count',
	'ANY',
	function(x, ..., level=level, na.rm=TRUE) 
		callGeneric( as.factor( .flatten(c(x,list(...))) ), level=level, na.rm=na.rm) 
)

#' @rdname aggregating-methods
#' @aliases count,logical-method
#' @export
setMethod('count',
	signature = c('logical'),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		callGeneric( as.factor(.flatten(c(x, list(...)))), level=level, na.rm=na.rm ) 
)

#' @rdname aggregating-methods
#' @aliases count,factor-method
#' @export
setMethod('count',
	signature = 'factor',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		if (! level %in% levels(x) ) {
			level = levels(x) [as.numeric(level)]
		}
		result <- sum( x == level, na.rm=na.rm ) 
		names(result) <- paste('count', level, sep=".")
		return(result)
	}
)

#' @rdname aggregating-methods
#' @aliases count,data.frame-method
#' @export
setMethod( 
	"count", 
	signature=c("data.frame"),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		sapply(x, sum, level=level, na.rm=na.rm)
)

#' @rdname aggregating-methods
#' @aliases count,formula-method
#' @export
setMethod( 
	"count", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., level=level, na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			x <-  eval(.simple.part(x), data) 
			if (! level %in% levels(x) ) {
				level = levels(x) [as.numeric(level)]
			}
			result <- sum( x == level, na.rm=na.rm ) 
			names(result) <- paste('count', level, sep=".")
			return(result)
		} else {
			stop('Invalid formula type.  Perhaps you should try xtabs().')
			return( .mosaic_aggregate( x, data, FUN=count, ..., level=level, na.rm=na.rm ) )
		} 
	}
)

##########################################################################

#' @export
#' @docType methods
#' @rdname aggregating-methods
#'
#' @examples
#' prop(sex, data=HELPrct)
#' prop(sex, data=HELPrct, level='male')
#' prop(HELPrct$sex)
 
#' @export
setGeneric('prop',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		dots <- list(...)
		if ( length(dots) > 0 && is.data.frame( dots[[1]] ) ) {
			data <- dots[[1]]
			return(prop(eval( substitute(x), data), level=level, na.rm=na.rm))
		}
		standardGeneric('prop')
	}
)

#' @rdname aggregating-methods
#' @aliases prop,ANY-method
#' @export
setMethod(
	'prop',
	'ANY',
	function(x, ..., level=level, na.rm=TRUE) 
		callGeneric( as.factor( .flatten(c(x,list(...))) ), level=level, na.rm=na.rm) 
)

#' @rdname aggregating-methods
#' @aliases prop,logical-method
#' @export
setMethod('prop',
	signature = c('logical'),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		callGeneric( as.factor( .flatten(c(x,list(...))) ), level=level, na.rm=na.rm ) 
)

#' @rdname aggregating-methods
#' @aliases prop,factor-method
#' @export
setMethod('prop',
	signature = 'factor',
	function(x, ..., level=TRUE, na.rm=TRUE) {
		if (! level %in% levels(x) ) {
			level = levels(x) [as.numeric(level)]
		}
		result <- base::mean( x == level, na.rm=na.rm ) 
		names(result) <- paste('prop', level, sep=".")
		return(result)
	}
)

#' @rdname aggregating-methods
#' @aliases prop,data.frame-method
#' @export
setMethod( 
	"prop", 
	signature=c("data.frame"),
	function(x, ..., level=TRUE, na.rm=TRUE) 
		sapply(x, prop, level=level, na.rm=na.rm)
)

#' @rdname aggregating-methods
#' @aliases prop,formula-method
#' @export
setMethod( 
	"prop", 
	signature=c("formula"),
	function(x, data=parent.frame(), ..., level=level, na.rm=TRUE) {
		if( .is.simple.formula(x) ) {
			return( prop( eval( .simple.part(x), data, enclos=parent.frame()), 
							   ..., level=level, na.rm=na.rm ) )
		} else {
			stop('Invalid formula type.  Perhaps you should try xtabs().')
			return( .mosaic_aggregate( x, data, FUN=prop, ..., level=level, na.rm=na.rm ) )
		} 
	}
)

#' Compute standard deviation
#'
#' This computes the standard deviation as the square root of variance to avoid
#' direct use of \code{\link[stats]{sd}}.
#'
#' @seealso \code{\link[mosaic]{sd}}
#'
#' @param x a vector or formula
#'
#' @return a numeric containing the standard deviaiton
#'
#' @note The primary reason for this function is that \code{\link[stats]{sd}} generates warnings
#' when used with \code{\link[Hmisc]{summary.formula}} from the \code{lattice} package.
#'
#' @examples
#' x <- rnorm(10)
#' SD(x)
#' sd(x)
#' summary(age ~ substance, data=HELPrct, fun=SD)
#' @export

SD <- function(x) {
	sqrt(stats::var(x))
}


#' Check if formula
#' 
#' @param x an object
#' @return TRUE for a formula, FALSE otherwise, even if evaluation throws an error
#'
#' @rdname mosaic-internal
#' @keywords internal

.is.formula <- function(x)  
	tryCatch( inherits(x, 'formula'), error = function(e) {FALSE} )

#' Check for simple formula
#'
#' @param x a formula
#'
#' @return TRUE if formula has no left-hand side or a simple right-hand side 
#' (e.g., \code{NULL}, ., 1,  or 0)
#'
#' @rdname mosaic-internal
#' @keywords internal
.is.simple.formula <-  function(x){
     inherits(x, "formula") &&
         (length(x)==2 || is.null(x[[3]]) ||
          (length(x[[3]])==1 &&
          ((is.numeric(x[[3]]) && (x[[3]]==0 || x[[3]]==1)) ||  (all.names(x[[3]]) %in% c(".")))))
}

# This could use a better name and a better desription

#' Extract simple part from formula
#'
#' @param x a formula
#'
#' @return simple part of formula or NULL if formula is not simple
#'
#' @rdname mosaic-internal
#' @keywords internal

.simple.part <- function(x) {
	if (! .is.simple.formula(x) ) {
		return(NULL) 
	} else {
		return(x[[2]])
	}
}

#' Extract simple part from formula
#'
#' @param x an R container object
#' @return a vector containing items in \code{x}
#'
#' @rdname mosaic-internal
#' @keywords internal

.flatten <- function(x) {
    result <- c()
  for (item in x) result <- c(result, item)
  return(result)
}


#' Aggregate for mosaic
#'
#' Wrapper to modify aggregating behavior of \code{\link[Hmisc]{summary.formula}}
#'
#' @return  a data frame
#'
#' @rdname mosaic-internal
#' @keywords internal
.mosaic_aggregate <- function(x, data, FUN, overall=mosaic.par.get("aggregate.overall"), ...) {
	if (length(x) == 2 ) {
		return( data.frame( FUN (eval( x[[2]], data, enclos=parent.frame()) ) ) )
	} else {
		return( as.data.frame( 
			Hmisc::summary.formula( x, data, fun=FUN, overall=overall, method='cross',...) ) )
	}
	result <- Hmisc::summary.formula(x, data, fun=FUN, overall=overall, method=method, ...)
	result <- as.data.frame(oldUnclass(result))
	return(result)
}
