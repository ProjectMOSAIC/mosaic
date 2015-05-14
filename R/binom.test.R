#' Exact Tests for Proportions
#' 
#' The \code{binom.test} function
#' performs an exact test of a simple null hypothesis about the probability of success in a 
#' Bernoulli experiment from summarized data or from raw data.
#' The mosaic \code{binom.test} provides wrapper functions around the function of the same name in \pkg{stats}.
#' These wrappers provide an extended interface (including formulas).  
#' 
# @usage binom.test( x, n, p = 0.5, alternative = c("two.sided", "less", "greater"), conf.level = 0.95,...) 
#'
#' @param x  count of successes, length 2 vector of success and failure counts, a formula,
#'   			or a character, numeric, or factor vector containing raw data.
#' 
#' @param n  sample size (successes + failures) or a data frame 
#'   (for the formula interface) 
#' @param p probability for null hypothesis 
#' @param alternative  type of alternative hypothesis 
#' @param conf.level  confidence level for confidence interval 
#' @param success  level of variable to be considered success.  All other levels are 
#'   	considered failure.
#' @param data.name name for data.  If missing, this is inferred from variable names.
#' @param data a data frame (if missing, \code{n} may be a data frame)
#' @param ci.method a method to use for computing the confidence interval 
#'   (case insensitive and may be abbreviated).
#' @param ... additional arguments (often ignored) 
#' 
#' @return an object of class \code{htest}
#' 
#' @details
#' This is a wrapper around \code{\link{binom.test}} from the \code{base} package
#' to simplify its use when the raw data are available, in which case 
#' an extended syntax for \code{binom.test} is provided.
#' 
#' @note When \code{x} is a 0-1 vector, 0 is treated as failure and 1 as success. Similarly,
#' for a logical vector \code{TRUE} is treated as success and \code{FALSE} as failure.
#' 
#'
#' @seealso \code{\link[mosaic]{prop.test}}, \code{\link[stats]{binom.test}}
#' 
#' 
#' @examples
#' # Several ways to get a confidence interval for the proportion of Old Faithful
#' # eruptions lasting more than 3 minutes.
#' data(faithful)
#' binom.test(faithful$eruptions > 3)
#' binom.test(97, 272)
#' binom.test(c(97, 272-97))
#' faithful$long <- faithful$eruptions > 3
#' binom.test(faithful$long)
#' binom.test(~ long, faithful)
#' binom.test(~ long, faithful, ci.method="Wald")
#' binom.test(~ long, faithful, ci.method="Plus4")
#' 
#' @keywords stats
#' 
#' @rdname binom.test
#' @export
binom.test <- function( x, n, p = 0.5, 
                        alternative = c("two.sided", "less", "greater"), 
                        conf.level = 0.95, 
                        ci.method = c("Score", "Wald", "Agresti-Coull", "Plus4"), ...) 
{
  x_lazy <- lazyeval::lazy(x)
  n_lazy <- lazyeval::lazy(n)
  ci.method <- 
    match.arg(
      tolower(ci.method)[1], 
      choices = c("score", "wald", "agresti-coull", "plus4"))
  
  res <- update_ci(
    binom_test( x = x,
                 n = n,
                 p = p,
                 alternative = alternative, 
                 conf.level = conf.level, 
                 ...),
    method = ci.method
  )
  
  # update some slots based on lazyevaluated args
  if (res$data.name == "x out of n") {
    res$data.name <- paste(deparse(x_lazy$expr), "out of", deparse(n_lazy$expr))
  }
  res
}

setGeneric(
		   "binom_test",
		   function( x, n, p = 0.5, 
					alternative = c("two.sided", "less", "greater"), 
					conf.level = 0.95, ...) 
		   {
			   standardGeneric('binom_test')
		   }
		   )

## @aliases binom_test,ANY-method
setMethod(
		  'binom_test',
		  'ANY',
		  function(
				   x, n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, ...) 
		  {
			  stats::binom.test( x=x, n=n , p = p,
								alternative = alternative,
								conf.level = conf.level,...) 
		  }
		  )

## @aliases binom_test,formula-method
setMethod(
		  'binom_test',
		  'formula',
		  function(
				   x, n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, data, ...) 
		  {
			  formula <- mosaic_formula(x, groups=NULL, max.slots=1)
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


			  binom_test(x, p=p, alternative=alternative, 
						 conf.level=conf.level, success=success, data.name=data.name, ...)
		  }
		  )

##  @aliases binom_test,numeric-method
setMethod(
		  'binom_test',
		  'numeric',
		  function( x,  n, p = 0.5, 
				   alternative = c("two.sided", "less", "greater"), 
				   conf.level = 0.95, success=NULL, data.name, ...) 
		  {
			  if ( length(x) == 1 ) {
				  result <-  stats::binom.test(x=x, n=n, p=p, alternative=alternative,
											   conf.level=conf.level) 
				  result$data.name <- paste( deparse(substitute(x)), "out of", deparse(substitute(n)) )
				  return(result)
			  }

			  if ( length(x) == 2 ) {
				  result <-  stats::binom.test(x=x[1], n=base::sum(x), p=p, alternative=alternative,
											   conf.level=conf.level) 
				  result$data.name <- deparse(substitute(x))
				  return(result)
			  }

			  if (missing(data.name)) { 
				  data.name <- deparse(substitute(x)) 
			  }
			  if (is.null(success) && all(x %in% c(0,1))) success <- 1
			  binom.test(x=factor(x), p=p, alternative=alternative, 
						 conf.level=conf.level, 
						 success=success, 
						 data.name=data.name, ...)
		  }
		  )

## @aliases binom_test,character-method
setMethod(
		  'binom_test',
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

## @aliases binom_test,logical-method
setMethod(
		  'binom_test',
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

## @aliases binom_test,factor-method
setMethod(
		  'binom_test',
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
			  count <- base::sum(x==success)
			  n <- length(x)
			  result <- stats::binom.test( x=count, n=n , p = p,
										  alternative = alternative,
										  conf.level = conf.level, ...) 
			  result$data.name <- data.name
			  return(result)
		  }
		  )
