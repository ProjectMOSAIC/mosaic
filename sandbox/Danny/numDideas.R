# Strategy: 
# The various dfdx, d2fdx2, d2fdxdy functions create a new function.
# The new function grabs the function f constructed from the formula
# and grabs the arguments, adding and subtracting the finite-difference step h
# as appropriate, then evaluating f at the new points to find the finite difference.

numD <- function(formula, ..., .hstep=NULL) {
  # translate the formula into a function
  f <- makeFunction(formula, ...)
  # find the variables with respect to which the derivative is to be taken
  # keeping repeated names so that 2nd derivs can be spotted.
  dvars <- all.vars(rhs(formula), unique=FALSE) 
  # What sort of derivative?
  if (length(dvars)==1) { #Simple first derivative 
    res <- dfdx( f, dvars[1],h=ifelse(is.null(.hstep),0.000001,.hstep))
    formals(res) <- c(formals(f))
    return(res) 
  }
  if (length(dvars==2) & dvars[1]==dvars[2]) { # Second unmixed partial 
    res <- d2fdx2( f, dvars[1],h=ifelse(is.null(.hstep),0.0001,.hstep))
    formals(res) <- formals(f)
    return(res)
  }
  if (length(dvars)==2) { # mixed partial
    res <- d2fdxdy(f,dvars[1],dvars[2],h=ifelse(is.null(.hstep),0.0001,.hstep))
    formals(res) <- formals(f)
    return(res)
  }
  if (length(dvars)>2){
    stop("Order greater than 2 not yet implemented.")
  }
}
# ===============
set.left.right <- function(C,var,h) {
  # C, L, R are center, left, and right of the interval respectively
  C <- C[-1] # drop the function name
  C[[var]] <- eval.parent( C[[var]], n=2)
  L <- C; 
  R <- C; 
  if( var %in% names(C)) { # Change the appropriate variable
    L[[var]] <- L[[var]] + h # left side of interval
    R[[var]] <- R[[var]] - h # right side of interval
  }
  return(list(L=L,R=R,C=C))
}
# ================
set.4corners <- function(C,var1,var2,h) {
  # C is the center
  # RU, RB, LU, LB are the right-upper, right-bottom, left-upper and left-bottom corners
  if( var2 %in% names(C) ) C[[var2]] <- eval.parent( C[[var2]], n=2)
  sides <- set.left.right(C,var1,h)
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
  return(list(RU=RU,RB=RB,LU=LU,LB=LB,C=C))
}

# =================
dfdx <- function(f,var,h) { # first order partial
  res <- function() {
    H <- set.left.right(as.list(match.call()),var,h)
    (do.call( f, H$L ) - do.call(f, H$R))/(2*h)
  }
  formals(res) <- formals(f) 
  return(res)
}
# ==============
d2fdxdy <- function(f,var1,var2,h) { # second order mixed partial
  res <- function() {
    H <- set.4corners(as.list(match.call()),var1,var2,h)
    (do.call(f,H$LU)+do.call(f,H$RB)-(do.call(f,H$LB)+do.call(f,H$RU)))/(4*h^2)
  }
  formals(res) <- formals(f) 
  return(res)
}
# =============
d2fdx2 <- function(f,var,h) { # second order unmixed partial
  res <- function() {
    H <- set.left.right(as.list(match.call()),var,h)
    (do.call( f, H$L ) + do.call(f,H$R) - 2*do.call(f, H$C))/(h^2)
  }
  formals(res) <- formals(f) 
  return(res)
}
# ==========================


### TESTS ####
do.tests = function(){
  too.different = function(x,y,tol=.001){abs(x-y) > tol}
  # Simple tests
  g = numD( a*x^2 ~ x, a=10 )
  if(too.different( g(3), 60 )) stop("Test 1")
  g = numD( a*x^2*y^2 ~ x&y, a=10)
  if(too.different( g(x=3,y=5), 600 )) stop("Test 2")
  g = numD( a*x^2*y^2 ~ x&x, a=10)
  if(too.different( g(x=3,y=5), 500)) stop("Test 3a")
  g = numD( a*x^2*y^2 ~ y&y, a=10)
  if(too.different( g(x=3,y=5), 180)) stop("Test 3b")
  g = numD( a*x^2*y^2 ~ a&a, a=10)
  if(too.different( g(x=3,y=5), 0)) stop("Test 3c")
  ## Construct the derivative in a function, then return it
  f = function(n) {
    numD( b*a*x^2*y^2 ~ x & y, a=.25,b=n) 
  }
  g = f(10)
  if(too.different(g(1,1), 10)) stop("Test 4a")
  if(too.different(g(1,1,b=20), 20)) stop("Test 4b")
  if(too.different(g(1,1,a=2,b=20), 160)) stop("Test 4c")
  
  
}


