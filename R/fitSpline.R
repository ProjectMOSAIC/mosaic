
#' Fit splines to data
#'
#' These functions create mathematical functions from data, using splines.
#'
#' @param formula a formula.  Only one quantity is allowed on the left-hand side, the
#' output quantity
#' @param data a data frame in which `formula` is evaluated.
#' @param type type of splines to use; one of 
#' `"linear"`, `"cubic"`, `"natural"` (cubic with linear tails, the default), 
#' or `"polynomial"`.
#' @param degree parameter for splines when `type` is `"polynomial"`.  
#' 1 is locally linear, 2 is locally quadratic, etc.
#' @param df degrees of freedom (used to determine how many knots should be used)
#' @param knots a vector of knots
#' @param \dots additional arguments passed to spline basis functions
#' ([splines::ns()] and [splines::bs()]).
#'
#' @return a function of the explanatory variable
#'
#' @seealso [splines::bs()]  and [splines::ns()] for the bases used to generate the splines.
#'
#' @examples
#' f <- fitSpline( weight ~ height, data=women, df=5 )
#' xyplot( weight ~ height, data=women )
#' plotFun(f(height) ~ height, add=TRUE)
#'
#' g <- fitSpline( length ~ width, data = KidsFeet, type='natural', df=5 )
#' h <- fitSpline( length ~ width, data = KidsFeet, type='linear', df=5 )
#' xyplot( length ~ width, data = KidsFeet, col='gray70', pch=16)
#' plotFun(g, add=TRUE, col='navy')
#' plotFun(h, add=TRUE, col='red')
#' @export

fitSpline <- function( formula, data=parent.frame(), 
			df = NULL,
			knots = NULL,
			degree = 3,
 			type=c('natural','linear','cubic','polynomial'),
			...) {
  
  rlang::check_installed('splines')

	type <- match.arg(type)

    xnames <- all.vars(rhs(formula))
    ynames <- all.vars(lhs(formula))
	if (length(xnames) != 1 || length(ynames) != 1) 
    	stop("Sorry: Doesn't handle multiple variables (yet).")
	x <- get( xnames, data )
	y <- get( ynames, data )

	method  <- switch(type,
					natural="natural",
					linear="linear",
					polynomial="polynomial",
					cubic='cubic'
					)
    if (method == 'natural') {
		if (is.null(knots)) {
			model <- lm( y ~ splines::ns(x, df=df, ...) )
		} else {
			model <- lm( y ~ splines::ns(x, df=df, knots=knots, ...) )
		}
    } else {
		if (method == 'linear') degree=1
		if (method == 'cubic') degree=3
		if (is.null(knots)) {
			model <- lm( y ~ splines::bs(x, df=df, degre=degree, ...) )
		} else {
			model <- lm( y ~ splines::bs(x, df=df, knots=knots, degre=degree, ...) )
		}
    }

	result <- function(x) {}
	    body(result) <- parse(text = paste("return(predict(model, newdata=data.frame(", 
            paste("x= ", xnames, collapse = ",", sep = ""), 
            "), ...))"))
        formals(result) <- eval(parse(text = paste("as.pairlist(alist(", 
            paste(xnames, "= ", collapse = ",", sep = ""), ", ...=))")))
		environment(result) <- list2env( c( as.list(environment(result)), 
								       list(knots=knots, df=df, degree=degree, model=model)) )
    	attr(result, "coefficients") <- coef(model)
    return(result)
}

