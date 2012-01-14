#' Exact Tests for Proportions
#' 
#' The mosaic \code{binom.test} provides wrapper functions around the function of the same name in \pkg{stats}.
#' These wrappers provide an extended interface (including formulas).  \code{binom.test} 
#' performs an exact test of a simple null hypothesis about the probability of success in a 
#' Bernoulli experiment from summarized data or from raw data.
#' 
#' @usage binom.test( x, n, p = 0.5, alternative = c("two.sided", "less", "greater"), conf.level = 0.95,...) 
#'
#' @param x  count of successes, length 2 vector of success and failure counts, a formula,
#'   			or a character, numeric, or factor vector containing raw data.
#' 
#' @param n  sample size (successes + failures) or a data frame 
#'   (for the formula interface) 
#' @param p  probability for null hypothesis 
#' @param alternative  type of alternative hypothesis 
#' @param conf.level  confidence level for confidence interval 
#' @param success  level of variable to be considered success.  All other levels are 
#'   	considered failure.
#' @param data.name name for data.  If missing, this is inferred from variable names.
#' @param data a data frame (if missing, \code{n} may be a data frame)
#' @param \dots  additional arguments (often ignored) 
#' 
#' @return an object of class \code{htest}
#' 
#' @details
#' This is a wrapper around \code{\link{binom.test}} from the \code{base} package
#' to simplify its use when the raw data are available, in which case 
#' an extended syntax for \code{binom.test} is provided.
#' 
#' @author Randall Pruim (\email{rpruim@@calvin.edu})
#'
#' @seealso \code{\link[mosaic]{prop.test}}, \code{\link[stats]{binom.test}}
#' 
#' 
#' @export
#' @examples
#' # Several ways to get a confidence interval for the proportion of Old Faithful
#' # eruptions lasting more than 3 minutes.
#' binom.test( faithful$eruptions > 3 )
#' binom.test(97,272)
#' binom.test(c(97,272-97))
#' faithful$long <- faithful$eruptions > 3
#' binom.test( faithful$long )
#' binom.test( ~long , faithful )
#' 
#' @keywords stats

#' @rdname binom.test
#' @usage binom.test( x, n, p = 0.5, alternative = c("two.sided", "less", "greater"), conf.level = 0.95,...) 
#' @export
#'
setGeneric(
		   "binom.test",
		   function( x, n, p = 0.5, 
					alternative = c("two.sided", "less", "greater"), 
					conf.level = 0.95,...) 
		   {
			   standardGeneric('binom.test')
		   }
		   )

#' @rdname binom.test
#' @aliases binom.test,ANY-method
setMethod(
		  'binom.test',
		  'ANY',
		  function(
				   x, n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95,...) 
		  {
			  stats::binom.test( x=x, n=n , p = p,
								alternative = alternative,
								conf.level = conf.level,...) 
		  }
		  )

#' @rdname binom.test
#' @aliases binom.test,formula-method

setMethod(
		  'binom.test',
		  'formula',
		  function(
				   x, n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, data, ...) 
		  {
			  formula <- x
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
			  groups <- form$groups
			  subscr <- form$subscr
			  cond <- form$condition
			  x <- form$right
			  if (length(cond) == 0) {
				  cond <- list(gl(1, length(x)))
			  }


			  binom.test(x, p=p, alternative=alternative, 
						 conf.level=conf.level, success=success, data.name=data.name, ...)
		  }
		  )

#' @rdname binom.test
#' @aliases binom.test,numeric-method
setMethod(
		  'binom.test',
		  'numeric',
		  function( x,  n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if ( length(x) == 1 ) {
				  result <-  stats::binom.test(x=x, n=n, p=p, alternative=alternative,
											   conf.level=conf.level) 
				  result$data.name <- paste( deparse(substitute(x)), "and", deparse(substitute(n)) )
				  return(result)
			  }

			  if ( length(x) == 2 ) {
				  result <-  stats::binom.test(x=x[1], n=sum(x), p=p, alternative=alternative,
											   conf.level=conf.level) 
				  result$data.name <- deparse(substitute(x))
				  return(result)
			  }

			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }

			  binom.test(x=factor(x), p=p, alternative=alternative, 
						 conf.level=conf.level, 
						 success=success, 
						 data.name=data.name, ...)
		  }
		  )

#' @rdname binom.test
#' @aliases binom.test,character-method
setMethod(
		  'binom.test',
		  'character',
		  function(
				   x,  n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  binom.test(x=factor(x), p=p, alternative=alternative, 
						 conf.level=conf.level, 
						 success=success, 
						 data.name=data.name, ...)
		  }
		  )

#' @rdname binom.test
#' @aliases binom.test,logical-method
setMethod(
		  'binom.test',
		  'logical',
		  function(
				   x,  n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  binom.test(x=factor(x, levels=c('TRUE','FALSE')), p=p, alternative=alternative, 
						 conf.level=conf.level, 
						 success=success, 
						 data.name=data.name, ...)
		  }
		  )

#' @rdname binom.test
#' @aliases binom.test,factor-method
setMethod(
		  'binom.test',
		  'factor',
		  function(
				   x,  n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  if ( missing(success) || is.null(success) ) {
				  success <- levels(x)[1]
			  }
			  x <- x [!is.na(x)]
			  count <- sum(x==success)
			  n <- length(x)
			  result <- stats::binom.test( x=count, n=n , p = p,
										  alternative = alternative,
										  conf.level = conf.level, ...) 
			  result$data.name <- data.name
			  return(result)
		  }
		  )
