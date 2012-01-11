#' Derivative and Anti-derivative operators
#' 
#' Operators for computing derivatives and anti-derivatives as 
#' functions.
#'
#' @rdname Calculus
#'
#' @param expr a formula. The right side specifies the variable(s) with which to 
#'   carry out the integration or differentiation.  On the left side should be 
#'   an expression or a function that returns a numerical vector 
#'   of the same length as its argument.  The expression can contain unbound variables.
#'
#' @param ..h..  horizontal distance between points used for secant slope
#'   calculation in \code{D()}.  This is used only if a symbolic derivative is not possible or 
#'   if \code{numerical=TRUE}.  The odd name, \code{..h..}, is there to avoid
#'   conflicts with unbound variables in the formula f.
#'   
#' @param symbolic a logical indicating whether symbolic differentiation should be attempted
#'   
#' @param numerical opposite of symbolic available for convenience
#'   
#' @param method For first-order numerical derivatives, whether to use a
#' centered difference or a right-facing difference.
#'   
#' @param \dots Default values to be given to unbound variables in the expression \code{expr}.  
#' See examples.  
#' 
#' @return For derivatives, the return value is a function of the variable(s)
#' of differentiation, as well as any other symbols used in the expression.  Thus,
#' \code{D(A*x^2 + B*y ~ x + y)} will compute the mixed partial with respect to x
#' then y (that is, \eqn{\frac{d^2 f}{dy\;dx}}{d2f/dydx}).  The returned value will be a function of x and y,
#' as well as A and B.  In evaluating the returned function, it's best to used the
#' named form of arguments, to make sure that you have the order right. 
#' 
#' @details
#' \code{D} attempts to find a symbolic derivative for simple expressions, but
#' will provide a function that is a numerical derivative if the attempt at
#' symbolic differentiation is unsuccessful.  The symbolic derivative can be of
#' any order (although the expression may become unmanageably complex).  The
#' numerical derivative is limited to first or second-order partial derivatives
#' (including mixed partials).
#' 
#' \code{antiD} always returns a numerical integral.  
#' 
#' For integrals, the return value is a function NOT of the variable (only
#' one!) of integration, but of \code{to} and \code{from}, the upper and lower
#' bounds of the interval of integration.
#' 
#' Since the return value is a function, the numerical value of the integral or
#' derivative can be found by evaluating that function against its inputs.
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu}) 
#' 
#' @export
#' @examples
#' D(sin(t) ~ t)
#' D(A*sin(t) ~ t )
#' D(A*sin(2*pi*t/P) ~ t, A=2, P=10) # default values for parameters.
#' f <- D(A*x^3 ~ x + x, A=1) # 2nd order partial -- note, it's a function of x
#' f(x=2)
#' f(x=2,A=10) # override default value of parameter A
#' g = D(f(x=t, A=1)^2 ~ t)  # note: it's a function of t
#' g(t=1) 
#' gg <- D(f(x=t,A=B)^2 ~ t, B=10)  # note: it's a function of t and B
#' gg(t=1)
#' gg(t=1, B=100)
#' 
#' @keywords calculus 
#' 

D <- function(expr, ..., ..h..=NULL, symbolic = TRUE, numerical=!symbolic, method=c("center","right")){
	#vals = list(...)
	sexpr <- substitute(expr)
	fm <- mosaic:::.createMathFun(sexpr=sexpr, ...)

	.doD(fm, numerical=numerical, method=method, ..h..=..h..,...)
}

.doD <- function( fm, ..h..=NULL, numerical=FALSE, method="center",...){
	vals <- list(...)
	# see if the expression is simple enough to use the symbolic method
	.ops <- setdiff( unique(all.names(fm$sexpr)), unique(all.vars(fm$sexpr)))
	.allowed.ops <- c("+","-","*","/","^","(", "exp", "log", "sin", "cos",
					  "tan", "sinh", "cosh", "sqrt", "pnorm", "dnorm", "asin", "acos", "atan",
					  "gamma", "lgamma", "digamma", "trigamma")
	.can.do.symbolic <- all(.ops %in% .allowed.ops)
	if (!numerical & .can.do.symbolic) {
		.df <- tryCatch(.d.symbolic(fm), error = function(e){NULL})
		if( !is.null(.df) ) return(.df)
		warning("Could not do derivative symbolically.  Returning numerical derivative.")
	}

	# do the derivative numerically
	# Check whether there are repeated variables in the RHS of the formula
	fm$names <- fm$RHS[fm$RHS %in% fm$names]
	# information to pass along to the differentiating functions
	needed <- list(sexpr=fm$sexpr, names=fm$names, h=..h..)

	if(length(fm$names)==1){ # a first-order derivative
		..h.. <- ifelse( is.null(..h..), 1e-4, ..h.. )
		needed$h <- ..h..
		if(method[1]=="right") .df <- .d1.right
		if(method[1]=="center") .df <- .d1.center
	}
	else{
		if(length(fm$names)==2) {
			..h.. <- ifelse( is.null(..h..), 1e-4, ..h.. )
			needed$h <- ..h..
			if( length(unique(fm$names))==1 ) .df <- .d2.xx #repeated w.r.t. same variable.
			else .df <- .d2.xy #mixed derivative
		}
		else if(length(fm$names)>2) stop("Handles only 1st- and 2nd-order derivatives.")
	}

	# Assign the arguments to the function
	fargs <- c(list())
	# put the arguments in an "alist" so that they are unbound 
	one <- list()
	two <- list()
	tmp1 <- unique(fm$names)
	if( length(tmp1) > 0) {
		tmp <- paste( "alist( ", 
					 paste(tmp1, "=",collapse=",",sep=""),")")
		one <- eval(parse(text=tmp))    
	}
	tmp2 <- all.vars(fm$sexpr,functions=FALSE)
	tmp2 <- setdiff(tmp2,tmp1)
	if( length(tmp2) > 0) {
		tmp <- paste( "alist( ", 
					 paste(tmp2, "=",collapse=",",sep=""),")")
		two <- eval(parse(text=tmp))    
	}
	fargs <- c(one,two,fargs)
	fargs[names(vals)] <- vals

	# EXCEPTIONS for 
	# global variables which are not accessible
	# if they are contained in the formals list
	fargs["pi"] <- NULL # let pi stand for pi
	# Pass the sexpr expression and name information to the function as a default
	fargs[["..args"]] <- needed


	formals(.df) <- fargs #formals(fm$fun)
	environment(.df) <- parent.frame()  # set the environment where the function will look for the expression
	attr(.df,"mosaicType") <- "Numerical finite-difference process"
	return(.df) 
}
# =====================
# various functions for performing derivatives
.d1.right <- function() {
	..h.. <- ..args$h
	..baseline <- eval(..args$sexpr)
	..vvv <- get(..args$names[1])
	# to avoid round-off error in h 
	..temp <- ..vvv + ..h..
	..h.. <- ..temp-..vvv
	assign(..args$names[1], ..vvv+..h..)
	return( (eval(..args$sexpr) - ..baseline)/..h..)
}
# =====
.d1.center <- function() {
	..h.. <- ..args$h
	..vvv <- get(..args$names[1])
	# to avoid round-off error in h 
	..temp <- ..vvv + ..h..
	..h.. <- ..temp-..vvv
	assign(..args$names[1], ..vvv + ..h..)
	..right <- eval(..args$sexpr)
	assign(..args$names[1], ..vvv - ..h..)
	..left <- eval(..args$sexpr)
	return( (..right - ..left)/(2*..h..))
}
#=============
.d2.xx <- function() {
	..h.. <- ..args$h
	..vvv <- get(..args$names[1])
	assign(..args$names[1], ..vvv)
	..center <- eval(..args$sexpr)
	# to avoid round-off error in h 
	..temp <- ..vvv + ..h..
	..h.. <- ..temp-..vvv
	assign(..args$names[1], ..vvv + ..h..)
	..right <- eval(..args$sexpr)
	assign(..args$names[1], ..vvv - ..h..)
	..left <- eval(..args$sexpr)
	return( ((..right+..left)-2*..center)/(..h..^2))
}
# ===============
.d2.xy <- function() {
	..h.. <- ..args$h
	..one <- get(..args$names[1])
	..two <- get(..args$names[2])
	# to avoid round-off error in h 
	..temp <- ..one + ..h..
	..h.one.. <- ..temp-..one
	..temp <- ..two + ..h..
	..h.two.. <- ..temp - ..two
	# Evaluate at 4 points on grid Lu, Ru, Ld, Rd
	assign(..args$names[1], ..one + ..h.one..) #move right
	assign(..args$names[2], ..two + ..h.two..) #move up
	..ru <- eval(..args$sexpr)
	assign(..args$names[2], ..two - ..h.two..) #move down
	..rd <- eval(..args$sexpr)
	assign(..args$names[1], ..one - ..h.one..) #move left
	assign(..args$names[2], ..two + ..h.two..) #move up
	..lu <- eval(..args$sexpr)
	assign(..args$names[2], ..two - ..h.two..) #move down
	..ld <- eval(..args$sexpr)

	return( ((..ru-..rd)/..h.two.. -(..lu-..ld)/..h.two..)/(4*..h.one..))
}
# ===================
# Symbolic derivative
.d.symbolic <- function(.f ) { #argument is list from .createMathFun
	fform <- function(){}
	formals(fform) <- formals(.f$fun)
	sexpr <- .f$sexpr
	wrtNames <- .f$RHS[ .f$RHS %in% .f$names ] # differentiate with respect to these
	for (j in 1:length(wrtNames)) {
		df <- deriv(sexpr, namevec=wrtNames[j], function.arg=fform)
		bd <- as.character(body(df))
		final.expr <- gsub(".grad.+<-", "",bd[length(bd)-2])
		# substitute in the values of .value, .expr1, and so on
		for (k in (length(bd)-4):2 ) {
			nm <- gsub(" <-.+$","", bd[k])
			val <- gsub("^.+<- ","",bd[k])
			final.expr <- gsub(nm,paste("(",val,")",sep=""), final.expr)
		}
		sexpr <- parse(text=final.expr)
	}
	body(df) <- parse(text=final.expr)
	return(df)
}


#' @rdname Calculus
#'
#' @param from Default value for the lower bound of the interval of
#' integration.  This can be set at the time the integral function is invoked.
#' @param to Default value for the upper bound of the interval of integration.
#' This can be set at the time the integral function is invoked (and usually is). 
#' 
#' @return For anti-derivatives, a decorated function (eventually this will be an object of a new class).
#' @note
#' A major redesign of these functions is planned that will (1) refactor code so that the objects involved
#' are S4 objects, and (2) allow \code{antiD} to return a single value if both \code{to} and \code{from} are specified.
#' @export
#' @examples
#' F <- antiD( A*exp(-k*t^2 ) ~ t, A=1, k=0.1)
#' F(from=-Inf, to=0)
#' F(from=-Inf, to=Inf)

antiD <- function(expr, from=0, to=NULL, ...){
	vals <- list(...)
	sexpr <- substitute(expr)
	# If it's an expression
	foo <- .createMathFun(sexpr=sexpr, ...) 
	if( length(foo$names) != 1 ) {
		stop("This function works only with a single variable of integration.")
	}
	finput <- function(.x) {
		assign(foo$names[1], .x)
		return( eval(foo$sexpr))
	}
	needed <- list(sexpr=foo$sexpr, names=foo$names) # data passed to the function by arguments
	..foutput <- .antiD.x

	if( any( c("to","from") %in% c(foo$names,names(formals(foo$fun)))) )
		stop("Can't use a variable called 'to' or 'from' in a function being integrated.")
	starting.args <- alist(to=)
	original.args <- formals(foo$fun)
	fargs <- c(starting.args, original.args)

	# Construct the argument list so that unbound variables have no default values.  
	tmp2 <- all.vars(foo$sexpr)
	tmp2 <- setdiff(tmp2,names(fargs))
	one <- list()
	if( length(tmp2) > 0) {
		tmp <- paste( "alist( ", 
					 paste(tmp2, "=",collapse=",",sep=""),")")
		one <- eval(parse(text=tmp))    
	}
	fargs <- c(fargs, one, list(from=from))

	fargs[names(vals)] <- vals
	fargs[foo$names] <- NULL 
	# EXCEPTIONS for 
	# global variables which are not accessible
	# if they are contained in the formals list
	fargs["pi"] <- NULL # let pi stand for pi

	# Pass the sexpr expression and name information to the function as a default
	fargs[["..args"]] <- needed

	environment(..foutput) <- parent.frame()
	formals(..foutput) <- fargs
	attr(..foutput,"mosaicType") <- "Numerical integration process"
	return(..foutput) 
}

# ===============================
.antiD.x <- function() { # really a function of the upper limit: "to"
	..finput <- function(.x) { # Create the function in this environment
		assign(..args$names[1], .x)    
		return( eval(..args$sexpr) )
	}  
	# handle the case where to is fixed and from is assigned to multiple values
	..multiplier <- 1
	if( length(from) > 1 & length(to) == 1 ){
		..temp <- to
		to <- from
		from <- ..temp
		..multiplier <- -1
	}
	# handle situation where both from and to are a set of values.
	if( length(from)>1 & length(to)>1 ){
		if( length(from)!=length(to) ) stop("Either fix 'from' or set it to the same length as 'to'")
		..res <- rep(0,length(to))
		for (..k.. in 1:length(to)) {
			..res[..k..] <- integrate(..finput,from[k],to[k])$value
		}
		return(..res)
	}
	..val0 <- integrate(..finput, from, to[1])$value
	if (length(to) == 1) {
		return(..multiplier*..val0)
	}
	..res <- rep(..val0, length(to))
	for (..k.. in 2:length(..res)) {
		..res[..k..] <- integrate(..finput, to[..k.. - 1], to[..k..])$value
	}
	..res <- cumsum(..res)
	return(..multiplier*..res)
}
