#' Generate a natural-looking function
#'
#' Produce a random function that is the sum of Gaussian random variables
#'
#' @param vars a formula; the LHS is empty and the RHS indicates the variables used 
#' for input to the function (separated by &)
#'
#' @param seed seed for random number generator, passed to \code{\link{set.seed}}.
#' @param n the number of Gaussians.  By default, this will be selected randomly.
#' @return a function with the appropriate number of inputs
#'
#' @details
#' \code{rfun} is an easy way to generate a natural-looking but random function with ups and downs
#' much as you might draw on paper.  In two variables, it provides a good way to produce
#' a random landscape that is smooth.
#' Things happen in the domain -5 to 5. The function is pretty flat outside of that.
#' Use \code{seed} to create a fixed function that will be the same for everybody
#'
#' @rdname rfun
#' @name rfun
#' @keywords random
#' @aliases rfun rpoly2
#'
#' @examples
#' f <- rfun( ~ u & v)
#' plotFun(f(u,v)~u&v,u=range(-5,5),v=range(-5,5))
#' myfun <- rfun(~ u & v, seed=1959)
#' g <- rpoly2( ~ x&y&z, seed=1964)
#' plotFun(g(x,y,z=2)~x&y,xlim=range(-5,5),ylim=range(-5,5))
#' @export
#' 
rfun <- function(vars=~x&y, seed=NULL, n=0) {
  if( !is.null(seed) ) set.seed(seed)
  if( class(vars) != "formula" )
    stop("Must provide a formula, e.g. ~x&y, to identify the variables")
  nmaxes <- ifelse(n==0, ceiling( runif(1,min=4,max=10)), n)
  varnames <- all.vars(vars)
  nvars <- length(varnames)
  locs <- list()
  for (k in 1:nvars) locs[[k]] <- runif(nmaxes, min=-3,max=3)
  signsmax <- runif(nmaxes,min=3,max=10)*sign( runif(nmaxes,min=-1,max=1) )
  xscales <- runif( nmaxes, min=.1, max=5 )
  # The functions are being created by hand for n=1,2,3.
  # For larger n, they are computed
  if( nvars == 1 ) {
    f <- function() {
      x <- eval(parse(text=varnames[1]))
      res <- 0  
      for(k in 1:nmaxes){
        res <- res +  signsmax[k]*exp(-(xscales[k]*(x-locs[[1]][k])^2)/9) 
      }
      return(res)
    }
  }
  if( nvars == 2 ) {
    f <- function() {
      x <- eval(parse(text=varnames[1]))
      y <- eval(parse(text=varnames[2]))
      res <- 0  
      for(k in 1:nmaxes){
        res <- res +  signsmax[k]*exp(-(xscales[k]*(x-locs[[1]][k])^2 + (y-locs[[2]][k])^2)/9) 
      }
      return(res)
    }
  }
  if( nvars == 3 ) {
    f <- function() {
      x <- eval(parse(text=varnames[1]))
      y <- eval(parse(text=varnames[2]))
      z <- eval(parse(text=varnames[3]))
      res <- 0  
      for(k in 1:nmaxes){
        res <- res +  signsmax[k]*exp(-(xscales[k]*(x-locs[[1]][k])^2 + (y-locs[[2]][k])^2 + (z-locs[[3]][k])^2)/9) 
      }
      return(res)
    }
  }
  if( nvars > 3 ) {
    f <- function() {
      x <- eval(parse(text=varnames[1]))
      res <- 0  
      for(k in 1:nmaxes){
        foo <- xscales[k]*(x-locs[[1]][k])^2
        for(j in 2:nvars) {
          x <- eval(parse(text=varnames[j]))
          foo <- foo + (x-locs[[j]][k])^2
        }
        res <- res +  signsmax[k]*exp(-foo/9) 
      }
      return(res)
    }
  }
  tmp <- paste( "alist( ", paste(varnames, "=",collapse=",",sep=""),")")
  tmp <- eval(parse(text=tmp))
  formals(f) <- tmp
  return(f)
}
 
#' random 2nd degree polynomials
#'
#' \code{rpoly2} generates a random 2nd degree polynomial  (as a function)
#'
#' @rdname rfun
#' @inheritParams rfun
#'
#' @return a function defined by a 2nd degree polynomial
#' with coefficients selected randomly according to a Unif(-1,1) distribution.
#'
#' @keywords random
#' @details These functions are particularly useful for teaching calculus.
#' @export

rpoly2 <- function(vars=~x&y,seed=NULL){
  if( !is.null(seed) ) set.seed(round(seed))
  if( class(vars) != "formula" )
    stop("Must provide a formula, e.g. ~x&y, to identify the variables")
  varnames <- all.vars(vars)
  if( length(vars)==2) {
    # no left-hand side of the formula (as is sensible)
    # So put a placeholder on the left-hand side
    vars[[3]] <- vars[[2]]
    vars[[2]] <- as.name(varnames[1])
  }
  fres <- makeFun(vars) # function to return
  
  # construct the body of a function
  nvars <- length(varnames)
  # How many coefficients in a 2nd-order polynomial (without intercept)?
  constantval <- runif(1,min=-1,max=1)
  coefslin <- runif(nvars,min=-1,max=1)
  coefsquad <- runif(nvars,min=-1,max=1)
  coefscross <- matrix(runif(nvars*nvars, min=-1,max=1),nrow=nvars)
  f <- function() {
    givenvars <- as.list(match.call())[-1] # arguments as a list
    if( nvars != length(givenvars)) 
      stop("Not all inputs specified.")
    for( k in 1:nvars )
      givenvars[[k]] <- eval.parent(givenvars[[k]],n=1)
    res <- constantval    
    for (k in 1:nvars) {
      x <- givenvars[[k]]
      res <-  res + coefslin[k]*x + coefsquad[k]*x^2
      if( k<(nvars-1)){
        for (j in (k+1):nvars) {
          res <- res + coefscross[k,j]*x*givenvars[[j]]
        }
      }
    }
    return(res)
    }
  
  body(fres) <- body(f)
  environment(fres) <- environment()
  return(fres)
}
