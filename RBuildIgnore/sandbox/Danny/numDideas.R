#' Numerical Derivatives
#'
#' Constructs the numerical derivatives of mathematical expressions
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @rdname numD
#' @name numD
#' @aliases numD
#'
#' @param formula a mathematical expression (see examples and \code{\link{plotFun}})
#' @param \dots additional parameters, typically default values for mathematical parameters
#' @param .hstep numerical finite-difference step (default is 1e-6 or 1e-4 for first and second-order derivatives, respectively)
#' @param add.h.control arranges the returned function to have a \code{.hstep} argument that cann be used to demonstrate convergence and error
#'
#' @return a function implementing the derivative as a finite-difference approximation
#'
#' @details
#' Uses a simple finite-difference scheme to evaluate the derivative.  The function created
#' will not contain a formula for the derivative.  Instead, the original function is stored
#' at the time the derivative is constructed and that original function is re-evaluated at the 
#' finitely-spaced points of an interval.  If you redefine the original function, that won't affect
#' any derivatives that were already defined from it.
#' Numerical derivatives, particularly high-order ones, are unstable.  The finite-difference parameter
#' \code{.hstep} is set, by default, to give reasonable results for first- and second-order derivatives.
#' It's tweaked a bit so that taking a second derivative by differentiating a first derivative
#' will give reasonably accurate results.  But, 
#' if taking a second derivative, much better to do it in one step to preserve numerical accuracy.
#' 
#' @Note WARNING: In the expressions, do not use variable names beginning with a dot, particularly \code{.f} or \code{.h}
#'
#' @seealso \code{\link{D}}, \code{\link{symbolicD}}, \code{\link{makeFun}}, \code{\link{antiD}}, \code{\link{plotFun}}
#'
#' @examples
#' g = numD( a*x^2 + x*y ~ x, a=1)
#' g(x=2,y=10)
#' gg = numD( a*x^2 + x*y ~ x&x, a=1)
#' gg(x=2,y=10)
#' ggg = numD( a*x^2 + x*y ~ x&y, a=1)
#' ggg(x=2,y=10)
#' h = numD( g(x=x,y=y,a=a) ~ y, a=1)
#' h(x=2,y=10)
#' f = numD( sin(x)~x, add.h.control=TRUE)
#' plotFun( foo(3,.hstep=h)~h, hlim=range(.00000001,.000001))
#' ladd( panel.abline(cos(3),0))

# Strategy: 
# The various dfdx, d2fdx2, d2fdxdy functions create a new function.
# The new function grabs the function f constructed from the formula
# and grabs the arguments, adding and subtracting the finite-difference step h
# as appropriate, then evaluating f at the new points to find the finite difference.

numD <- function(formula, ..., .hstep=NULL,add.h.control=FALSE) {
  formulaEnv = environment(formula) # where did the formula come from?
  # translate the formula into a function
  f <- makeFun(formula, ...)
  environment(f) <- formulaEnv  # was parent.frame()
  # find the variables with respect to which the derivative is to be taken
  # keeping repeated names so that 2nd derivs can be spotted.
  dvars <- all.vars(rhs(formula), unique=FALSE) 
  # What sort of derivative?
  if (length(dvars)==1)  #Simple first derivative 
    res = dfdx( f, dvars[1], .h=ifelse(is.null(.hstep), 0.000001, .hstep))
  else if (length(dvars==2) & dvars[1]==dvars[2]) # Second unmixed partial 
    res = d2fdx2( f, dvars[1], .h=ifelse(is.null(.hstep), 0.0001, .hstep))
  else if (length(dvars)==2) # mixed partial
    res = d2fdxdy(f, dvars[1], dvars[2], .h=ifelse(is.null(.hstep), 0.0001, .hstep))

  if (length(dvars)>2){
    stop("Order greater than 2 not yet implemented.")
  }
  
  if( add.h.control )
    formals(res) = c(formals(res),list(.hstep=0.00001))
  return(res)
}
# ===============
setInterval <- function(C, wrt, h) {
  # C, L, R are center, left, and right of the interval respectively
  C <- C[-1] # drop the function name
  if( ".hstep" %in% names(C)) C$.hstep=NULL #.hstep doesn't go to the function
  C[[wrt]] <- eval.parent( C[[wrt]], n=3)
  L <- C; 
  R <- C; 
  if( wrt %in% names(C)) { # Change the appropriate variable
    L[[wrt]] <- L[[wrt]] - h # left side of interval
    R[[wrt]] <- R[[wrt]] + h # right side of interval
  }
  return(list(L=L, R=R, C=C))
}
# ================
setCorners <- function(C, var1, var2, h) {
  # C is the center
  # RU, RB, LU, LB are the right-upper, right-bottom, left-upper and left-bottom corners
  if( var2 %in% names(C) ) C[[var2]] <- eval.parent( C[[var2]], n=3)
  sides <- setInterval(C, var1, h)
  RU = sides$R
  RB = sides$R
  LU = sides$L
  LB = sides$L
  if( var2 %in% names(C) ){
    RU[[var2]] <- RU[[var2]]+h
    RB[[var2]] <- RB[[var2]]-h
    LU[[var2]] <- LU[[var2]]+h
    LB[[var2]] <- LB[[var2]]-h
  }
  return(list(RU=RU, RB=RB, LU=LU, LB=LB, Center=sides$C))
}
# =================
# Formal arguments are named to avoid conflicts with the contents of the mathematical function
# whose derivative is sought.  Similarly for the others: d2fdx2, d2fdxdy
dfdx <- function(.function, .wrt, .hstep) { # first order partial
  res <- function() numerical.first.partial(.function, .wrt, .hstep, match.call())
  #  H <- setInterval(as.list(match.call()), .wrt, .h)
  #  (do.call( .f, H$R ) - do.call(.f, H$L))/(2*.h)
  formals(res) <- formals(.function) 
  return(res)
}
# ==============
d2fdxdy <- function(.function, .var1, .var2, .hstep) { # second order mixed partial
  res <- function() numerical.mixed.partial(.function, .var1, .var2, .hstep, match.call())
    #H <- setCorners(as.list(match.call()), var1, var2, h)
    #(do.call(f, H$RU) + do.call(f, H$LB) - (do.call(f, H$RB) + do.call(f, H$LU)))/(4*h^2)
  formals(res) <- formals(.function) 
  return(res)
}
# =============
d2fdx2 <- function(.function, .wrt, .hstep) { # second order unmixed partial
  res <- function() numerical.second.partial(.function,.wrt,.hstep,match.call())
  #H <- setInterval(as.list(match.call()), wrt, h)
  #(do.call( f, H$R ) + do.call(f, H$L) - 2*do.call(f, H$C))/(h^2)
  formals(res) <- formals(.function) 
  return(res)
}
# =============
numerical.first.partial = function(f,wrt,h,av) {
  H <- setInterval(as.list(av), wrt, h)
  (do.call( f, H$R,quote=TRUE ) - do.call(f, H$L,quote=TRUE))/(2*h)
}
numerical.second.partial = function(f,wrt,h,av) {
  H <- setInterval(as.list(av), wrt, h)
  (do.call( f, H$R ) + do.call(f, H$L) - 2*do.call(f, H$C))/(h^2)
}
numerical.mixed.partial = function(f,var1,var2,h,av){
  H <- setCorners(as.list(av), var1, var2, h)
  (do.call(f, H$RU) + do.call(f, H$LB) - (do.call(f, H$RB) + do.call(f, H$LU)))/(4*h^2)
}
# ==========================


### TESTS ####
testf = makeFun(a*x^2 ~x, a=1)
testg = numD(testf(f)~f)

do.tests = function(){
  too.different = function(x, y, tol=.001){abs(x-y) > tol}
  # Simple tests
  g = numD( a*x^2 ~ x, a=10 )
  if(too.different( g(3), 60 )) stop("Test 1")
  g = numD( a*x^2*y^2 ~ x&y, a=10)
  if(too.different( g(x=3, y=5), 600 )) stop("Test 2")
  g = numD( a*x^2*y^2 ~ x&x, a=10)
  if(too.different( g(x=3, y=5), 500)) stop("Test 3a")
  g = numD( a*x^2*y^2 ~ y&y, a=10)
  if(too.different( g(x=3, y=5), 180)) stop("Test 3b")
  g = numD( a*x^2*y^2 ~ a&a, a=10)
  if(too.different( g(x=3, y=5), 0)) stop("Test 3c")
  ## Construct the derivative in a function, then return it
  f = function(n) {
    numD( b*a*x^2*y^2 ~ x & y, a=.25, b=n) 
  }
  g = f(10)
  if(too.different(g(1, 1), 10)) stop("Test 4a")
  if(too.different(g(1, 1, b=20), 20)) stop("Test 4b")
  if(too.different(g(1, 1, a=2, b=20), 160)) stop("Test 4c")
  # Iterative derivatives
  g = numD( a*x^3 ~ x, a=10, .hstep=.001)
  gg = numD( g(y)~y, .hstep=.001 )
  ggg = numD( gg(x)~x, .hstep=.001 )
  if(too.different(gg(3), 180, tol=.01)) stop("Test 5a")
  if(too.different(ggg(3), 60, tol=.01)) stop("Test 5b")
  # Function composition
  ff = makeFun( sin(x)~x )
  h = numD( ff(x)~x )
  hh = numD( sin(y)*ff(y)~y )
  if(too.different( hh(y=3), 2*sin(3)*cos(3))) stop("Test 6a")
  hhh = numD( sin(y)*h(y)~y ) 
  if(too.different( hhh(y=3), cos(3)*cos(3) - sin(3)*sin(3) )) stop("Test 6b")
  
  # Use internal variables to make sure there are no conflicts
  g = numD( f^2*h~f&h)
  if(too.different(g(f=2, h=1), 4)) stop("Test 7a")
  
  # Check a function defined outside this one
  testgg = numD( testg(y)~y)
  if(too.different(testgg(2),2)) stop("Test 8a")
  
  #Control of .hstep
  f = numD( sin(x)~x, add.h.control=TRUE)
  if(too.different(f(3, .hstep=1), -.83305)) stop("Test 9a")
}


