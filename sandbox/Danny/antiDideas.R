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
                          as.list(match.call())[-1],
                          formals())
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
  # make sure expressions get evaluated
  for(k in 1:length(av)) {
    if(inherits(av[[k]],c("call","name"))) 
      av[[k]] = eval.parent(av[[k]],n=2)
  }
  for(k in 1:length(args)) {
    if(inherits(args[[k]],c("call","name"))) 
      args[[k]] = eval.parent(args[[k]],n=2)
  }
  # Extract the limits from the argument list
  av2 = c(av, args) # combine the actual arguments with the formals

  # to make sure that default values are included
  vi.from <- inferArgs(wrt, av2, defaults=alist(val=NaN), 
                       variants = c("from",".from"))$val
  vi.to <- inferArgs(wrt, av2, defaults=alist(val=NaN), 
                     variants = c("to",".to"))$val
  if( any(is.nan(vi.to)) | any(is.nan(vi.from))) stop("Integration bounds not given.")
  # and delete them from the call
  av[[paste(wrt,".from",sep="")]] <- NULL
  av[[paste(wrt,".to",sep="")]] <- NULL
  newf <- function(vi){
    av[[wrt]] = vi
    do.call(f,av,quote=TRUE) + 0*vi #make the same size as vi
  }
  integrateFun( newf, vi.from, vi.to)
  # NEED TO ADD TOLERANCE
}
# =======================
integrateFun = function(f, from, to){
  # handle the case where to is fixed and from is assigned to multiple values
  multiplier <- 1
  if( length(from) > 1 & length(to) == 1 ){
    temp <- to
    to <- from
    from <- temp
    multiplier <- -1
  }
  # handle situation where both from and to are a set of values.
  if( length(from)>1 & length(to)>1 ){
    if( length(from)!=length(to) ) stop("Either fix 'from' or set it to the same length as 'to'")
    res <- rep(0,length(to))
    for (k in 1:length(to)) {
      res[k] <- integrate(f,from[k],to[k])$value
    }
    return(res)
  }
  val0 <- integrate(f, from, to[1])$value
  if (length(to) == 1) {
    return(multiplier*val0)
  }
  res <- rep(val0, length(to))
  for (k in 2:length(res)) {
    res[k] <- integrate(f, to[k - 1], to[k])$value
  }
  res <- cumsum(res)
  return(multiplier*res)
}


