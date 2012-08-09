
#' Derivative and Anti-derivative operators
#' 
#' Operators for computing derivatives and anti-derivatives as 
#' functions.
#'
#' @rdname Calculus
#'
#' @param formula A formula. The right side specifies the variable(s) with which to 
#'   carry out the integration or differentiation.  On the left side should be 
#'   an expression or a function that returns a numerical vector 
#'   of the same length as its argument.  The expression can contain unbound variables.
#'
#' @param \dots Default values to be given to unbound variables in the expression \code{expr}.  
#' See examples.#'  Note that in creating anti-derivative functions, 
#' default values of "from" and "to" can be assigned.  They are to be written with
#' the name of the variable as a prefix, e.g. \code{y.from}.
#'
#' @param .hstep  horizontal distance between points used for secant slope
#'   calculation in numerical derivatives. 
#'   
#' @param add.h.control logical indicating whether the returned derivative function
#'   should have an additional parameter for setting .hstep.  Meaningful only for numerical
#'   derivatives.
#'   
#' @return For derivatives, the return value is a function of the variable(s)
#' of differentiation, as well as any other symbols used in the expression.  Thus,
#' \code{D(A*x^2 + B*y ~ x + y)} will compute the mixed partial with respect to x
#' then y (that is, \eqn{\frac{d^2 f}{dy\;dx}}{d2f/dydx}).  The returned value will be a function of x and y,
#' as well as A and B.  In evaluating the returned function, it's best to use the
#' named form of arguments, to ensure the order is correct.
#' 
#' @details 
#' \code{D} attempts to find a symbolic derivative for simple expressions, but
#' will provide a function that is a numerical derivative if the attempt at
#' symbolic differentiation is unsuccessful.  The symbolic derivative can be of
#' any order (although the expression may become unmanageably complex).  The
#' numerical derivative is limited to first or second-order partial derivatives
#' (including mixed partials).
#' \code{antiD} will attempt simple symbolic integration but if it fails
#' it will return a numerically-based anti-derivative.
#' 
#' \code{antiD} returns a function with the same arguments as the 
#' expression passed to it.  The returned function is the anti-derivative 
#' of the expression, e.g., antiD(f(x)~x) -> F(x).  
#' To calculate the integral of f(x), use F(to) - F(from). 
#' 
#' @export
#' @examples
#' D(sin(t) ~ t)
#' D(A*sin(t) ~ t )
#' D(A*sin(2*pi*t/P) ~ t, A=2, P=10) # default values for parameters.
#' f <- D(A*x^3 ~ x + x, A=1) # 2nd order partial -- note, it's a function of x
#' f(x=2)
#' f(x=2,A=10) # override default value of parameter A
#' g <- D(f(x=t, A=1)^2 ~ t)  # note: it's a function of t
#' g(t=1) 
#' gg <- D(f(x=t, A=B)^2 ~ t, B=10)  # note: it's a function of t and B
#' gg(t=1)
#' gg(t=1, B=100)
#' f <- makeFun(x^2~x)
#' D(f(cos(z))~z) #will look in user functions also
#' 
D <- function(formula, ..., .hstep=NULL,add.h.control=FALSE){
  tryCatch( return( stats::D(formula, ...) ), error=function(e) {}  )
  
  formulaEnv = environment(formula) # where was the formula made?
  #Try to construct a symbolic derivative
  res = try(symbolicD(formula, ...), silent=TRUE)
  #Failed?  Do it numerically  
  if( inherits(res, "try-error") ){ # first symbolic attempt unsuccessful
    expandedForm <-try(expandFun(formula), silent=TRUE)
    if(!inherits(expandedForm, "try-error"))
      newformula <- expandedForm$formula
    res = try(symbolicD(newformula, ...), silent=TRUE)
    if( inherits(res, "try-error") ) # second symbolic attempt unsuccessful
      res = numD( formula, ..., .hstep=.hstep, add.h.control=add.h.control)
    else #extracted function correctly
      formals(res) = expandedForm$formals
  }
  else # it's generated from symbolicD
    environment(res) = formulaEnv # function should refer to environment of the formula
  return(res)
}

# ============================
#' @rdname Calculus
#'
#' @param lower.bound for numerical integraion, the lower bound used
#' 
#' @param force.numeric If \code{TRUE}, a numerical integral is performed even when a 
#' symbolic integral is available.
#' 
#' @return a function of the same arguments as the original expression with a 
#' constant of integration set to zero by default, named "C", "D", ... depending on the first 
#' such letter not otherwise in the argument list.
#' @export
#' @examples
#' antiD( a*x^2 ~ x)
#' antiD( A/x~x )
#' F <- antiD( A*exp(-k*t^2 ) ~ t, A=1, k=0.1)
#' F(t=Inf)
#' one = makeFun(1~x&y)
#' by.x = antiD( one(x=x, y=y) ~x)
#' by.xy = antiD(by.x(x=sqrt(1-y^2), y=y)~y)
#' 4*by.xy(y=1) #area of quarter circle
antiD <- function(formula, ..., lower.bound=0, force.numeric=FALSE){
  wrt <- all.vars(rhs(formula), unique=FALSE) # "with respect to" variable name
  if (length(wrt) != 1)  stop("Integration with respect to multiple variables not supported directly.")
  
  if (!force.numeric){ # Try symbolic integral
    res = try(symbolicInt(formula, ...), silent=TRUE)
    if (!inherits(res, "try-error") ) return(res) 
  }
  # Do integral numerically
  f <- makeFun(formula, ..., strict.declaration=FALSE)
  res <- makeAntiDfun(f, wrt, lower.bound, 1e-6)
  return(res)
}
# ===================
# The function returned by antiD will take the same arguments as
# f, but will split one of the variables into a "to" and "from" component. 
# The "from" will have a default value of 0 or otherwise inherited from 
# the call to antiD
# The variable of integration will be called "viName"
#' @rdname Calculus
#'
#' @param .function function to be integrated
#' @param .wrt character string naming the variable of integration
#' @param from default value for the lower bound of the integral region
# I don't want this function to be exported.
makeAntiDfun <- function(.function, .wrt, from, .tol=.Machine$double.eps^0.25) {
  resargs <- formals(.function) 
  
  intC <- LETTERS[-(1:2)][!LETTERS[-(1:2)]%in% names(resargs)][1]
  if (length(intC)==0) intC <- paste("ConstantOfIntegration",runif(1),sep="")
  resargs[intC] <- 0
  # Create a new function of argument .vi that will take additional
  # arguments
  .newf <- function(.vi,.av){
    .av[[.wrt]] = .vi
    do.call(.function,.av,quote=TRUE) + 0*.vi  # make the same size as vi
  }
  # Create the numerical integral
  res <- function(){
    numerical.integration(.newf,.wrt,as.list(match.call())[-1],formals(),
                          from,ciName=intC, .tol) 
  }
  
  formals(res) <- c(resargs)
  ## Vectorize at the end
  # return(Vectorize(res))
  return(res)
}
# =============
#' @rdname Calculus
#'
#' @param f a function
#' @param wrt character string naming a variable: the var. of integration
#' @param av a list of the arguments passed to the function calling this
#' @param args default values (if any) for parameterss
#' @param vi.from the the lower bound of the interval of integration
#' @param ciName character string giving the name of the symbol for the constant of integration
#' @param .tol Numerical tolerance.  See stats::integrate
#' 
#' @note This function is not intended for direct use.  It packages
#' up the numerical anti-differentiation process so that the contents
#' of functions produced by \code{antiD} look nicer to human readers.
#' @export
#'
numerical.integration <- function(f,wrt,av,args,vi.from, ciName="C",.tol) {
  # We are about to do the numerics.  At this point, every
  # variable should have a numerical binding.  Just in case some
  # are still expressions, go through the list and evaluate them
  for(k in 1:length(av)) av[[k]] = eval.parent(av[[k]],n=2)
  av2 = c(av, args) # combine the actual arguments with the formals
  # to make sure that default values are included
  # Extract the limits from the argument list
  vi.to <- inferArgs(wrt, av2, defaults=alist(val=NaN), 
                     variants = c("","to",".to"))$val
  # If they are calls, turn them into values.  Redundant with loop above
  if( any(is.nan(vi.to)) | any(is.nan(vi.from))) stop("Integration bounds not given.")
  # and delete them from the call
  av[[paste(wrt,".from",sep="")]] <- NULL
  av[[paste(wrt,".to",sep="")]] <- NULL
  initVal <- av2[[ciName]]
  av[[ciName]] <- NULL
  newf <- function(vi){
    av[[wrt]] = vi
    # make the same size as input vi
    f(vi,av) + 0*vi
  }
  # NEED TO ADD TOLERANCE
  
  multiplier <- 1
  if( length(vi.from) > 1 & length(vi.to) == 1 ){
    temp <- to
    to <- from
    from <- temp
    multiplier <- -1
  }
  # handle situation where both from and to are a set of values.
  if( length(vi.from)>1 & length(vi.to)>1 ){
    if( length(vi.from)!=length(vi.to) ) stop("Either fix 'from' or set it to the same length as 'to'")
    res <- rep(0,length(vi.to))
    for (k in 1:length(vi.to)) {
      res[k] <- stats::integrate(newf,vi.from[k],vi.to[k],rel.tol=.tol)$value
    }
    return(res+initVal)
  }
  val0 <- stats::integrate(newf, vi.from, vi.to[1],rel.tol=.tol)$value
  # work around a bug in integrate()
  if( vi.to[1]==-Inf ) {
    # When "upper" limit is -Inf, the sign is wrong!  This might
    # change, so check that the bug still exists
    if( stats::integrate(function(x){dnorm(x)},lower=0,upper=-Inf,rel.tol=.tol)$val > 0) {
      # bug exists!
      val0 <- -val0
    }
  } 
  # add initial condition
  val0 <- val0 + initVal
  if (length(vi.to) == 1) {
    return(multiplier*val0)
  }
  res <- rep(val0, length(vi.to))
  for (k in 2:length(res)) {
    res[k] <- stats::integrate(newf, vi.to[k-1], vi.to[k],rel.tol=.tol)$value
  }
  res <- cumsum(res)
  return(multiplier*res)
}
