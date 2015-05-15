#' Exact and Approximate Tests for Proportions
#' 
#' The mosaic \code{prop.test} provides wrapper functions around the function of the same name in \pkg{stats}.
#' These wrappers provide an extended interface (including formulas).  
#' \code{prop.test} performs an approximate test of a simple null hypothesis about the 
#' probability of success in a Bernoulli or multinomial experiment
#' from summarized data or from raw data.
#' 
# @usage prop.test( x, n, p = NULL, alternative = c("two.sided", "less", "greater"), 
#' 					conf.level = 0.95, ...) 
#' 
#' @param x  count of successes, length 2 vector of success and failure counts, a formula,
#'   			or a character, numeric, or factor vector containing raw data.
#'     		
#' @param groups when \code{x} is a formula, \code{groups} can be used to 
#' compare groups.  (This can also be done using by placing both variables into
#' the formula.)  See the examples.
#' 
#' @param n  sample size (successes + failures) or a data frame 
#'   (for the formula interface) 
#' 
#' @param p  a vector of probabilities of success. 
#' The length of p must be the same as the number of groups specified by x, 
#' and its elements must be greater than 0 and less than 1.
#' 
#' @param alternative   character string specifying the alternative hypothesis, must be one of 
#' \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter. 
#' Only used for testing the null that a single proportion equals a given value, or that two proportions 
#' are equal; ignored otherwise.
#' 
#' @param conf.level confidence level of the returned confidence interval. Must be a single number 
#' between 0 and 1. Only used when testing the null that a single proportion equals a given value, 
#' or that two proportions are equal; ignored otherwise.
#'
#' @param success  level of variable to be considered success.  All other levels are 
#'   	considered failure.
#'
#' @param data.name name for data.  If missing, this is inferred from variable names.
#' 
#' @param data a data frame (if missing, \code{n} may be a data frame)
#' 
#' @param ... additional arguments (often ignored) 
#' 
#' @note When \code{x} is a 0-1 vector, 0 is treated as failure and 1 as success. Similarly,
#' for a logical vector \code{TRUE} is treated as success and \code{FALSE} as failure.
#'
#' @return an \code{htest} object
#' 
#' @details
#' This is a wrapper around \code{\link{prop.test}} to simplify its use
#' when the raw data are available, in which case 
#' an extended syntax for \code{prop.test} is provided.  
#' 
#' @seealso \code{\link[mosaic]{binom.test}}, \code{\link[stats]{prop.test}}
#' 
#' @examples
#' # Several ways to get a confidence interval for the proportion of Old Faithful
#' # eruptions lasting more than 3 minutes.
#' prop.test( faithful$eruptions > 3 )
#' prop.test(97,272)
#' faithful$long <- faithful$eruptions > 3
#' prop.test( faithful$long )
#' prop.test( ~long , faithful )
#' if (require(mosaicData)) {
#' prop.test( homeless ~ sex, data=HELPrct )
#' prop.test( ~ homeless | sex, data=HELPrct )
#' prop.test( ~ homeless, groups= sex, data=HELPrct )
#' }
#' 
#' @keywords stats

#'
#' @rdname prop.test
#' @export
 
prop.test <- function( x, n, p = NULL, 
          alternative = c("two.sided", "less", "greater"), 
          conf.level = 0.95, data = parent.frame(), ..., data.name) 
{
  x_lazy <- lazyeval::lazy(x)
  n_lazy <- lazyeval::lazy(n)
  data_lazy <- lazyeval::lazy(data)
  missing_n <- missing(n)
  x_eval <- tryCatch(
    lazyeval::lazy_eval(x_lazy, data),
    error = function(e) as.name(deparse(x_lazy$epr)) )
  
  # this list will later be converted to a string using the appropriate information
  # dependent upon which of the prop_test methods is called.  
  
  data.name <- list(x=x_lazy, n=n_lazy, data=data_lazy)
 
  if (missing_n) {
    prop_test(x_eval, p = p, alternative = alternative, 
            conf.level = conf.level, data=data, data.name = data.name, ...)
  } else {
    prop_test(x_eval, n, p = p, alternative = alternative, 
            conf.level = conf.level, data=data, data.name = data.name, ...)
  }
}

setGeneric(
		   "prop_test",
		   function( x, n, p = NULL, 
					alternative = c("two.sided", "less", "greater"), 
					conf.level = 0.95, ...) 
		   {
			   standardGeneric('prop_test')
		   }
		   )

## @aliases prop_test,ANY-method

setMethod(
  'prop_test',
  'ANY',
  function(
    x, n, p=NULL, 
    alternative = c("two.sided", "less", "greater"), 
    conf.level = 0.95, ..., data, data.name) 
  {
    dots <- list(...)
    res <- do.call(stats::prop.test, 
                   c(
                     list(x = x, n = n , p = p, alternative = alternative,
                          conf.level = conf.level), 
                     dots)
    )
    res
  }
)

## @aliases prop_test,formula-method

setMethod(
		  'prop_test',
		  'formula',
		  function(
				   x, n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, data=parent.frame(), groups=NULL, ...) 
		  {
			  formula <- mosaic_formula_q(x, groups=groups, max.slots=2)
			  missing.n <- missing(n)
			  missing.data <- missing(data)
			  dots <- list(...)
			  #    groups <- eval(substitute(groups), data, environment(formula))
			  #    subset <- eval(substitute(subset), data, environment(formula))
			  if (missing.n) { #  && !missing.data) {
			    form <- lattice::latticeParseFormula(formula, data, #subset = subset, #groups = groups,  
			                                         subscripts = TRUE, drop = TRUE)
			    if (missing(data.name)) {
			      data.name <- 
			        paste( deparse(substitute(data)), "$", form$right.name, sep="" )
			    } 
			    if (is.list(data.name)) {
			      data.name <- 
			        paste( deparse(data.name$data$expr), "$", form$right.name, sep="" )
			    }
			  } else {
			    form <- lattice::latticeParseFormula(formula, n, #subset = subset, #groups = groups,  
			                                         subscripts = TRUE, drop = TRUE)
			    if (missing(data.name)) {
			      data.name <- 
			        paste( deparse(substitute(n)), "$", form$right.name, sep="" )
			    }
			    if (is.list(data.name)) {
			      data.name <- 
			        paste( deparse(data.name$n$expr), "$", form$right.name, sep="" )
			    }
			    data <- n
			  }
			  # now data.name should be set and data should hold the data

			  #    groups <- eval(substitute(groups), data, environment(formula))
			  #    subset <- eval(substitute(subset), data, environment(formula))
			  groups <- form$groups
			  subscr <- form$subscr
			  cond <- form$condition
			  x <- form$right
      
        if (! is.null(form$left) && !is.null(form$condition) )
          stop("Formulas may not have both lhs and condition for prop.test.")
        
        if (! is.null(form$left) || !is.null(form$condition) ) {
          table_from_formula <-  tally( formula, data=data, margin=FALSE, format="count" )
          res <- stats::prop.test( t(table_from_formula), 
                             p=p,
                             conf.level=conf.level, 
                             alternative=alternative, 
                             ...)  
          res$data.name <- paste0("tally(", deparse(formula), ")")
          return(res)
        }
        
			  if (length(cond) == 0) {
				  cond <- list(gl(1, length(x)))
			  }

			  prop_test(x, p=p, alternative=alternative, 
						conf.level=conf.level, success=success, data.name=data.name, ...)
		  }
		  )

## @aliases prop_test,numeric-method

setMethod(
		  'prop_test',
		  'numeric',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, ..., data, data.name) 
		  {
			  # first hanlde case when n is provided
			  if ( !missing(n) ) {  
			    if (missing(data.name)) {
				    data.name <- paste( deparse(substitute(x)), "out of", deparse(substitute(n)) )
			    }
			    if (is.list(data.name)) {
				    data.name <- paste( deparse(data.name$x$expr), "out of", deparse(data.name$n$expr) )
			    }
				  result <-  stats::prop.test(x=x, n=n, p=p, alternative=alternative,
											  conf.level=conf.level,...) 
				  result$data.name <- data.name 
				  return(result)
			  }
        
        # when n is missing, treat the numbers as raw data rather than counts

			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
		    if (is.list(data.name)) {
				  data.name <- deparse(data.name$x$expr) 
		    }
		    # set a reasonable value for success if none given
        if (is.null(success)) {
          success <- 
            if (all(x %in% c(0, 1))) 1 else
              if (0 %in% x) 0 else 
                min(x, na.rm=TRUE)
        }
		    message(
		      paste("n is missing; treating x as raw data with success =", success)
		    )
			  prop_test(x=factor(x), p=p, alternative=alternative, 
						conf.level=conf.level, 
						success=success, 
						data.name=data.name, ...)
		  }
		  )

## @aliases prop_test,character-method

setMethod(
		  'prop_test',
		  'character',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, ..., data, data.name) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  if (is.list(data.name)) { 
				  data.name <- deparse(data.name$x$expr) 
			  }
			  prop_test(x=factor(x), p=p, alternative=alternative, 
						conf.level=conf.level, 
						success=success, 
						data.name=data.name, ...)
		  }
		  )

## @aliases prop_test,logical-method

setMethod(
		  'prop_test',
		  'logical',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, ..., data, data.name) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  if (is.list(data.name)) { 
				  data.name <- deparse(data.name$x$expr) 
			  }
			  prop_test(x=factor(x, levels=c('TRUE','FALSE')), p=p, alternative=alternative, 
						conf.level=conf.level, 
						success=success, 
						data.name=data.name, ...)
		  }
		  )

## @aliases prop_test,factor-method

setMethod(
		  'prop_test',
		  'factor',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, ..., data, data.name) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  if (is.list(data.name)) { 
				  data.name <- deparse(data.name$x$expr) 
			  }
			  if (is.null(success)) {
				  success <- levels(x)[1]
			  }
			  x <- x [!is.na(x)]
			  count <- sum(x==success)
			  n <- length(x)
			  result <- stats::prop.test( x=count, n=n , p = p,
										 alternative = alternative,
										 conf.level = conf.level, ...) 
			  result$data.name <- data.name
			  return(result)
		  }
		  )
