#' Exact and Approximate Tests for Proportions
#' 
#' The mosaic \code{prop.test} provides wrapper functions around the function of the same name in \pkg{stats}.
#' These wrappers provide an extended interface (including formulas).  
#' \code{prop.test} performs an approximate test of a simple null hypothesis about the 
#' probability of success in a Bernoulli or multinomial experiment
#' from summarized data or from raw data.
#' 
#' @usage prop.test( x, n, p = NULL, alternative = c("two.sided", "less", "greater"), 
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
#' @param \dots  additional arguments (often ignored) 
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
#' prop.test(c(97,272-97))
#' faithful$long <- faithful$eruptions > 3
#' prop.test( faithful$long )
#' prop.test( ~long , faithful )
#' prop.test( homeless ~ sex, data=HELPrct )
#' prop.test( ~ homeless | sex, data=HELPrct )
#' prop.test( ~ homeless, groups= sex, data=HELPrct )
#' 
#' @keywords stats
#' 

#'
#' @rdname prop.test
#' @export
#'
setGeneric(
		   "prop.test",
		   function( x, n, p = NULL, 
					alternative = c("two.sided", "less", "greater"), 
					conf.level = 0.95,...) 
		   {
			   standardGeneric('prop.test')
		   }
		   )

#' @rdname prop.test
#' @aliases prop.test,ANY-method
#' @usage
# \S4method{prop.test}{ANY}(x, n, p=NULL, alternative = c("two.sided", "less", "greater"), 
#'  conf.level = 0.95,...) 
setMethod(
		  'prop.test',
		  'ANY',
		  function(
				   x, n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95,...) 
		  {
			  stats::prop.test( x=x, n=n , p = p,
							   alternative = alternative,
							   conf.level = conf.level,...) 
		  }
		  )

#' @rdname prop.test
#' @aliases prop.test,formula-method
#' @usage
# \S4method{prop.test}{formula}( x, n, p=NULL, alternative = c("two.sided", "less", "greater"), 
#'				   conf.level = 0.95, success=NULL, data.name, data, groups=NULL, ...) 
setMethod(
		  'prop.test',
		  'formula',
		  function(
				   x, n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, data, groups=NULL, ...) 
		  {
			  formula <- mosaic_formula_q(x, groups=groups, max.slots=2)
			  missing.n <- missing(n)
			  missing.data <- missing(data)
			  dots <- list(...)
			  #    groups <- eval(substitute(groups), data, environment(formula))
			  #    subset <- eval(substitute(subset), data, environment(formula))
			  if (missing.n && !missing.data) {
				  form <- lattice::latticeParseFormula(formula, data, #subset = subset, #groups = groups,  
													   subscripts = TRUE, drop = TRUE)
				  if (missing(data.name)) {
					  data.name <- paste( deparse(substitute(data)), "$", form$right.name, sep="" )
				  }
			  } else {
				  form <- lattice::latticeParseFormula(formula, n, #subset = subset, #groups = groups,  
													   subscripts = TRUE, drop = TRUE)
				  if (missing(data.name)) {
					  data.name <- paste( deparse(substitute(n)), "$", form$right.name, sep="" )
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
          return( prop.test( t(table_from_formula), ...) ) 
        }
        
			  if (length(cond) == 0) {
				  cond <- list(gl(1, length(x)))
			  }

			  prop.test(x, p=p, alternative=alternative, 
						conf.level=conf.level, success=success, data.name=data.name, ...)
		  }
		  )

#' @rdname prop.test
#' @aliases prop.test,numeric-method
#' @usage
# \S4method{prop.test}{numeric}( x, n, p=NULL, alternative = c("two.sided", "less", "greater"), 
#'				   conf.level = 0.95, success=NULL, data.name, ...) 
setMethod(
		  'prop.test',
		  'numeric',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
		    if ( FALSE ) {  # no longer allowing this since it masks some stats::prop.test() behavior
		      result <-  stats::prop.test(x=x[1], n=sum(x), p=p, alternative=alternative,
		                                  conf.level=conf.level,...) 
		      result$data.name <- deparse(substitute(x))
		      return(result)
		    }
			  if ( !missing(n) ) {  # doing this if there is an n
				  result <-  stats::prop.test(x=x, n=n, p=p, alternative=alternative,
											  conf.level=conf.level,...) 
				  result$data.name <- paste( deparse(substitute(x)), "and", deparse(substitute(n)) )
				  return(result)
			  }
        
        # when n is missing, treat the numbers as raw data rather than counts

			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }

			  prop.test(x=factor(x), p=p, alternative=alternative, 
						conf.level=conf.level, 
						success=success, 
						data.name=data.name, ...)
		  }
		  )

#' @rdname prop.test
#' @aliases prop.test,character-method
#' @usage
# \S4method{prop.test}{character}( x, n, p=NULL, alternative = c("two.sided", "less", "greater"), 
#'				   conf.level = 0.95, success=NULL, data.name, ...) 
setMethod(
		  'prop.test',
		  'character',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  prop.test(x=factor(x), p=p, alternative=alternative, 
						conf.level=conf.level, 
						success=success, 
						data.name=data.name, ...)
		  }
		  )

#' @rdname prop.test
#' @aliases prop.test,logical-method
#' @usage
# \S4method{prop.test}{logical}( x, n, p=NULL, alternative = c("two.sided", "less", "greater"), 
#'				   conf.level = 0.95, success=NULL, data.name, ...) 
setMethod(
		  'prop.test',
		  'logical',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  prop.test(x=factor(x, levels=c('TRUE','FALSE')), p=p, alternative=alternative, 
						conf.level=conf.level, 
						success=success, 
						data.name=data.name, ...)
		  }
		  )

#' @rdname prop.test
#' @aliases prop.test,factor-method
#' @usage
# \S4method{prop.test}{factor}( x, n, p=NULL, alternative = c("two.sided", "less", "greater"), 
#'				   conf.level = 0.95, success=NULL, data.name, ...) 
setMethod(
		  'prop.test',
		  'factor',
		  function(
				   x,  n, p=NULL, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
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
