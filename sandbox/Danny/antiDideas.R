# new ideas for antiD

antiD <- function(formula, ...){
  wrt <- all.vars(rhs(formula), unique=FALSE) # "with respect to" variable name
  if (length(wrt) != 1)  stop("Integration with respect to multiple variables not supported directly.")
  f <- makeFunction(formula, ..., strict.declaration=FALSE)
  # NOTE: Don't use NULL as the default value.  Non-NULL is needed
  # so that the argument list gets created appropriately
  vi.from <- inferArgs( wrt, list(...), defaults=alist(val=0), 
                        variants = c("from",".from"))$val
  vi.to <- inferArgs( wrt, list(...), defaults=alist(val=NaN), 
                      variants = c("to",".to"))$val
  return(makeAntiDfun(f, wrt, vi.from, vi.to, 1e-6))
}
# ===================
# The function returned by antiD will take the same arguments as
# f, but will split one of the variables into a "to" and "from" component. 
# The "from" will have a default value of 0 or otherwise inherited from 
# the call to antiD
# The variable of integration will be called "viName"

makeAntiDfun <- function(.function, .wrt, from, to, .tol) { 
  # Combine default args with those given in the function call
  res <- function() {
    numerical.integration(.function,.wrt, 
                          as.list(match.call())[-1],formals())
  }
  resargs <- formals(.function) 
  resargs[[.wrt]] <- NULL
  limitsArgs = list()
  limitsArgs[[paste(.wrt,".to",sep="")]] <- to # should come first
  limitsArgs[[paste(.wrt,".from",sep="")]] <- from # should come second
  formals(res) <- c(limitsArgs,resargs)
  return(res)
}
# =============
numerical.integration <- function(f,wrt,av,args) {
  # Extract the limits from the argument list
  av2 = c(av, args) # combine the actual arguments with the formals
  # to make sure that default values are included
  vi.from <- inferArgs(wrt, av2, defaults=alist(val=NaN), 
                       variants = c("from",".from"))$val
  vi.to <- inferArgs(wrt, av2, defaults=alist(val=NaN), 
                     variants = c("to",".to"))$val
  browser()
  if( is.nan(vi.to) | is.nan(vi.from)) stop("Integration bounds not given.")
  # and delete them from the call
  av[[paste(wrt,".from",sep="")]] <- NULL
  av[[paste(wrt,".to",sep="")]] <- NULL
  newf <- function(vi){
    av[[wrt]] = vi
    do.call(f,av,quote=TRUE) + 0*vi #make the same size as vi
  }
  # NEED TO ADD TOLERANCE
  # Copy code from old antiD
  # But first test it out for scalar inputs.
  integrate(newf, vi.from, vi.to )$value
}


arglist = list(x.from=9, x.to=100)
vi.from = inferArgs( c("x"), arglist, defaults=alist(val=0), 
           variants = c("from",".from"))$val
vi.to = inferArgs( c("x"), arglist, defaults=alist(val=NULL), 
           variants = c("to",".to"))$val
print( c(vi.from, vi.to))