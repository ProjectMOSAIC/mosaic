
#' Fit splines to data
#'
#' These functions create mathematical functions from data, using splines.
#'
#' @param formula a formula.  Only one quantity is allowed on the left-hand side, the
#' output quantity
#' @param data a data frame in which \code{formula} is evaluated.
#' @param type type of splines to use; one of 
#' \code{"linear"}, \code{"cubic"}, \code{"natural"} (cubic with linear tails, the default), 
#' or \code{"polynomial"}.
#' @param degree parameter for splines when \code{type} is \code{"polynomial"}.  
#' 1 is locally linear, 2 is locally quadratic, etc.
#' @param df degrees of freedom (used to determine how many knots should be used)
#' @param knots a vector of knots
#' @param \dots additional arguments passed to spline basis functions
#' (\code{\link{ns}} and \code{\link{bs}}).
#'
#' @return a function of the explanatory variable
#'
#' @seealso \code{\link{bs}}  and \code{\link{ns}} for the bases used to generate the splines.
#'
#' @export
#' @examples
#' f <- fitSpline( weight ~ height, data=women, df=5 )
#' xyplot( weight ~ height, data=women )
#' plotFun(f(height) ~ height, add=TRUE)

fitSpline <- function( formula, data=parent.frame(), 
			df = NULL,
			knots = NULL,
			degree = 3,
 			type=c('natural','linear','cubic','polynomial'),
			...) {

	type <- match.arg(type)
    xnames <- all.vars(rhs(formula))
    ynames <- all.vars(lhs(formula))
	if (length(xnames) != 1 || length(ynames) != 1) 
    	stop("Sorry: Doesn't yet handle multiple variables (yet).")
	x <- get( xnames, data )
	y <- get( ynames, data )

	method  <- switch(type,
					natural="natural",
					linear="linear",
					polynomial="polynomial",
					cubic='cubic',
					periodic='periodic',
					monotonic='monoH.FC',
					interpolating='interpolating'
					)

	model <- switch(method,
					natural = lm( y ~ 1 + ns(x, knots=knots, df=df, ...), data=data ),
					polynomial = lm( y ~ 1 + bs(x, knots=knots, df=df, degree=degree, ...), data=data ),
					linear = lm( y ~ 1 + bs(x, knots=knots, df=df, degree=1, ...), data=data ),
					cubic = lm( y ~ 1 + bs(x, knots=knots, df=df, degree=3, ...), data=data ),
					NULL
					)

	result <- makeFun(model)
	environment(result) <- list2env( c( as.list(environment(result)), 
								       list(knots=knots, df=df, degree=degree)) )
	return(result)
}

